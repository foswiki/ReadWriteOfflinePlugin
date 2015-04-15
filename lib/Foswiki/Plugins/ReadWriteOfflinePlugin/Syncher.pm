# WRC466130775A3661534B79534655
#
# Copyright (C) 2006-2009 Crawford Currie http://wikiring.com
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# This notice must be retained in all copies and derivatives of
# this code.
#
# Support for synchronising two topics together.

package Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher;

use strict;
use Assert;
use Error qw(:try);
use List::Util ();
use Data::Dumper ();
use File::Path ();

use Foswiki::Plugins::ReadWriteOfflinePlugin::UserAgent ();
use Foswiki::Plugins ();
use Foswiki::Meta ();
use Foswiki::Merge ();
use Foswiki::Form ();
use Foswiki::Time ();

# %$TRACKINGCODE%

our $haveStorable;
our $details = 0; # set to 1 for detailed trace
our $SYNCH_ERROR = -1; # special rev no that flags a transfer error

BEGIN {
    eval 'use Storable ()';
    $haveStorable = !$@;
};

# Construct a new Syncher
# $session - ref to session object
# $lid = local ID, identifies this node in conflicts
# $direction - specifies if this syncher should receive, transmit or both
sub new {
    my ($class, $session, $lid, $direction) = @_;
    my $this = bless({}, $class);

    $direction ||= 'rxtx';
    $this->{session} = $session;
    # Next request number. Requests to the server are numbered, and
    # responses must carry the expected number.
    $this->{verify} = 0;

    # The object class of the store handler
    $this->{handlerClass} =
      $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{Store};

    $this->{localID} = $lid;
    $this->{receive} = ($direction =~ /rx/i);
    $this->{transmit} = ($direction =~ /tx/i);
    $this->{work_area} = Foswiki::Func::getWorkArea('ReadWriteOfflinePlugin');
    # LWP user agent for requests
    $this->{userAgent} =
      new Foswiki::Plugins::ReadWriteOfflinePlugin::UserAgent($this);
    # This tracks the (server) time the last message was received from the
    # server, and will be set after the first message is exchanged.
    $this->{serverTime} = 0;
    return $this;
}

sub finish {
    my $this = shift;
    $this->{session} = undef;
    $this->{userAgent} = undef;
}

our $SYNCH;

# Log a low-level event on this node. Low level events are file updates.
sub logEvent {
    my $this = shift;

    my $mess = Foswiki::Time::formatTime(time(), '$iso', 'gmtime').' ';
    foreach my $p (@_) {
        if (ref($p)) {
            pop(@$p) while (scalar(@$p) && !$p->[$#$p]);
            $mess .= join('/', @$p);
        } elsif (defined $p) {
            $mess .= $p;
        }
    }
    $mess .= "\n";
    my $log;
    my $f = $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{EventLog} ||
      "$Foswiki::cfg{DataDir}/RWOPEventLog.txt";
    if (open( $log, '>>', $f)) {
        print $log $mess;
        close( $log );
    } else {
        print STDERR $mess;
    }
}

# $rid = remote ID, identifies that node in conflicts
# $reporter = ref of object that implements report($string)
# Return 1 on success, 0 on failure. if the function fails, the
# remote server is *undefined*.
sub setRemoteServer {
    my ($this, $rid, $reporter) = @_;
    ASSERT($reporter->can('report'));
    my $rwop =
      \%{$Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{Servers}->{$rid}};

    foreach my $var qw(DefaultUrlHost ScriptUrlPath ScriptSuffix) {
        unless (defined $rwop->{$var}) {
            $reporter->report(
                "Configuration error - no $var defined for server $rid", 1);
            return 0;
        }
    }

    $this->{remoteID} = $rid;
    $this->{username} = $rwop->{Username};
    $this->{password} = $rwop->{Password};
    my $rdh = $rwop->{DefaultUrlHost};
    my $rsu = $rwop->{ScriptUrlPath};
    $rsu .= '/' if ($rsu && $rsu !~ m!/$!);
    my $rss = $rwop->{ScriptSuffix};
    $this->{restURL} = "$rdh${rsu}rest$rss/ReadWriteOfflinePlugin/";
    $this->{userAgent}->cookie_jar({
        file => "$this->{work_area}/$rid.cookie_jar"});

    my $state = 1;
    try {
        $this->getEnvFromRemote();
        $reporter->report("Connected to '$rid'");
    } otherwise {
        $reporter->report("Could not contact $rid: ".shift->{-text});
        $state = 0;
    };

    return $state;
}

# Synch a single web with the current remote node.
# $web = web to synch (same name on remote)
# $reporter = ref of object that implements report($string)
# $reset = true to ignore existing synch DB
sub synchWeb {
    my ($this, $lweb, $rweb, $reporter, $reset) = @_;
    ASSERT($reporter->can('report'), $reporter);

    # Load the synch DB for this web. The synch DB contains a map from
    # topic names to the rev at the time of the last synch on local
    # and remote. note that local and remote path names may be different
    # if the local and remote webs have different names.
    my $synchDBFile = "$this->{work_area}/$lweb.$this->{remoteID}.synchDB";
    $synchDBFile = Foswiki::Sandbox::untaintUnchecked($synchDBFile);

    # Make sure the synch DB exists, or can be created
    my $synch = { '!DBNAME!' => $synchDBFile };
    if (!-e $synchDBFile) {
        _writeDB($synch);
    } elsif (!-w $synchDBFile) {
        die "$synchDBFile is not writable";
    }

    unless ($reset) {
        if ($haveStorable) {
            $synch = Storable::retrieve($synchDBFile);
        } else {
            local $SYNCH;
            do $synchDBFile;
            $synch = $SYNCH;
        }
    }
    $synch->{'!DBNAME!'} = $synchDBFile;

    # Synch to the same web again unless a specific web was given
    $rweb ||= $synch->{'!WEBNAME!'} || $lweb;
    $synch->{'!WEBNAME!'} = $rweb;

    if ($reset) {
        _writeDB($synch);
    }

    $reporter->report(
        "Synching $this->{localID}:$lweb with $this->{remoteID}:$rweb");

    $this->{lastSynch} = $synch->{'!LAST SYNCH!'} || 0;

    $reporter->report("Last synch was at $this->{lastSynch}") if $details;
    $reporter->report("Synch DB is $synchDBFile") if $details;

    $this->_synchCollection([$lweb], [$rweb], $synch, $reporter, 1);

    # Issue highlighted by Michael Tempest of ATE.
    # Timestamp the synch with the server time when the synch ended.
    # An end timestamp is needed because files copied up to the server
    # will be timestamped on the server and a start timestamp would
    # predate them. On the reverse side a start timestamp is preferred
    # in the interests of avoiding race conditions (topics modified on
    # the server after the synch started but before the timestamp).
    # We use the synch end timestamp because even if changes occur on the
    # server side before the synch, the version numbers should allow
    # us to recover. The time is only important when topics are deleted.
    $reporter->report("Synch completed at $this->{serverTime}") if $details;
    $synch->{'!LAST SYNCH!'} = $this->{serverTime}
      if $this->{receive} and $this->{transmit};

    _writeDB($synch);

    $reporter->report(
        "$this->{localID}:$lweb synched with $this->{remoteID}:$rweb");
}

sub _writeDB {
    my $synch = shift;
    my $synchDBFile = $synch->{'!DBNAME!'};
    unless (-e $synchDBFile) {
        $synchDBFile =~ m#^(.*)/[^/]*$#;
        File::Path::mkpath($1);
    }
    if ($haveStorable) {
        Storable::store($synch, $synchDBFile);
    } else {
        open(F, ">$synchDBFile") ||
          die "Failed to open $synchDBFile for write: $!";
        my $dumper = Data::Dumper->new([$synch], ['SYNCH']);
        $dumper->Indent(0);
        print F $dumper->Dump();
        close(F);
    }
}

sub _synchCollection {
    my ($this, $localName, $remoteName, $synch, $reporter, $merge) = @_;

    $reporter->report('Synching collection '.join('.',@$localName)
                        .' with remote '.join('.',@$remoteName));

    # Get local and remote synch DBs
    my $local = $this->getContents(@$localName);
    my $remote = $this->getContentsFromRemote(@$remoteName);

    if ($details) {
        $reporter->report(
            "LOCAL: ".join(', ',map{"$_=$local->{$_}"}keys %$local), -1);
        $reporter->report(
            "REMOTE: ".join(', ',map{"$_=$remote->{$_}"}keys %$remote), -1);
    }
    # Do subwebs first
    my %webs = map { $_ => 1 } grep { $_ =~ s/^W!// }
      (keys %$remote, keys %$local);
    foreach my $n (sort keys %webs) {
        my @lname = @$localName;
        $lname[0] .= "/$n";
        my @rname = @$remoteName;
        $rname[0] .= "/$n";
        # Keep track of the end of the synch process
        $this->_synchCollection(\@lname, \@rname, $synch, $reporter, 0);
    }

    my %attachments = map { $_ => 1 } grep { /^A!/ }
      (keys %$remote, keys %$local);
    my %topics = map { $_ => 1 } grep { /^T!/ }
      (keys %$remote, keys %$local);
    $this->{objectsInCollection} = scalar(keys %attachments)
      + scalar(keys %topics);
    $this->{objectNumber} = 1;

    # Now attachments
    foreach my $n (sort keys %attachments) {
        $n =~ /^A!([^.]*)\.(.*)$/;
        my @lname = @$localName;
        push(@lname, ($1, $2));
        my @rname = @$remoteName;
        push(@rname, ($1, $2));
        $this->_synchVersionedObject(
            \@lname, \@rname, $n, $local, $remote, $synch, $reporter, 0);
    }

    # And finally topics
    foreach my $n (sort keys %topics) {
        my (undef, $leaf) = split(/!/, $n);
        my @lname = @$localName;
        push(@lname, $leaf);
        my @rname = @$remoteName;
        push(@rname, $leaf);
        $this->_synchVersionedObject(
            \@lname, \@rname, $n, $local, $remote, $synch, $reporter, 1);
    }

    $reporter->report("Finished ".join('.',@$localName));
}

sub _synchVersionedObject {
    my ($this, $localName, $remoteName, $key, $localVersions,
        $remoteVersions, $synch, $reporter, $merge) = @_;

    my $lsname = join('/', @$localName);
    my $rsname = join('/', @$remoteName);
    my $local = $localVersions->{$key} || 0;
    my $remote = $remoteVersions->{$key} || 0;
    # Get timestamps
    my $local_t = $localVersions->{"timestamp_$key"} || 0;
    my $remote_t = $remoteVersions->{"timestamp_$key"} || 0;

    $reporter->report(
        $this->{objectNumber}.' of '.$this->{objectsInCollection}.': '.
          'Synching versioned object '.
            $lsname.'@'.$local.
              ' with '.$rsname.'@'.$remote);
    $this->{objectNumber} ++;

    # the 'defined' test makes sure that local actually knew about
    # the object in the past
    if ($remote && !$local && defined $synch->{local}{$lsname}) {
        $reporter->report("...remote but not local") if $details;
        $reporter->report("...age ($remote_t - $this->{lastSynch})="
                            .($remote_t - $this->{lastSynch}))
          if $details;
        # Exists remotely, but not locally, and we didn't have an error
        # synching it before
        if ($this->{lastSynch} >= $remote_t
           && ($synch->{local}{$lsname} || 0) != $SYNCH_ERROR) {
            $reporter->report("...remote is old") if $details;
            if ($this->{transmit}) {
                # Thing was removed locally and has not been updated remotely
                # since the last synch. Delete the remote topic.
                my $act = "$this->{remoteID}:$rsname\@$remote"
                  . " (gone locally and not updated on $this->{remoteID})";
                $this->logEvent("Deleting $act");
                try {
                    # We pass $remote to make sure we don't delete the wrong
                    # version
                    $this->deleteVersionedObjectFromRemote(
                        $remoteName, $remote);
                    delete $synch->{local}{$lsname};
                    delete $synch->{remote}{$rsname};
                    _writeDB($synch);
                    $reporter->report(
                        'Deleted '.$this->{remoteID}.':'.$rsname.'@'.$remote.
                          ' (gone locally and not updated on '.
                            $this->{remoteID}.')', 1);
                } otherwise {
                    my $e = shift;
                    my $mess = $e->{-text} || '';
                    #print STDERR "RWOP0: $e->{-file}:$e->{-line}\n";
                    $reporter->report(
                        "Error while synching $lsname: $mess", 2);
                    $this->logEvent("** Error while deleting $act: $mess");
                };
            }
            else {
                $reporter->report(
                    'Did NOT delete '.$this->{remoteID}.':'
                      .$rsname.'@'.$remote.
                      ' (gone locally and not updated on '.
                        $this->{remoteID}
                          .') because sending changes is inhibited', 1);
            }
            return;
        }
        $reporter->report("...remote is new") if $details;
        # Otherwise thing is missing locally, but updated remotely since
        # the last synch (or there was an error during the last synch).
        # Copy it locally.
        if ($this->{receive}) {
            my $act = "$lsname from $this->{remoteID}:$rsname\@$remote";
            $this->logEvent('Creating ', $act);
            try {
                $synch->{local}{$lsname} =
                  $this->updateLocalFromRemote(
                      $localName, $local, $remoteName);
                $synch->{remote}{$rsname} = $remote;
                _writeDB($synch);
                $reporter->report(
                    "Created $lsname\@1 from "
                      ."$this->{remoteID}:$rsname\@$remote", 1);
            } otherwise {
                my $e = shift;
                my $mess = $e->{-text} || '';
                #print STDERR "RWOP1: $e->{-file}:$e->{-line}\n";
                $reporter->report("Error while synching $act: $mess", 2);
                # flag error for next synch
                $synch->{local}{$lsname} = $SYNCH_ERROR;
                _writeDB($synch);
                $this->logEvent("** Error while receiving $act: $mess");
            };
        }
        else {
            $reporter->report(
                "Did NOT create $lsname\@1 from $this->{remoteID}:$rsname\@$remote because receiving changes is inhibited",
                1);
        }
        return;
    }

    # Anything that has been touched locally since the last synch,
    # and does not exist on the remote.
    if ($local && !$remote) {
        if ($synch->{local}{$lsname} &&
              $synch->{local}{$lsname} == $local) {
            # Thing was there during last synch, and hasn't been updated
            # locally, so it must have been deleted on the remote node
            if ($this->{receive}) {
                my $act = "$lsname\@$local"
                  . "(gone on $this->{remoteID} and not updated locally)";
                $this->logEvent("Removing $act");
                try {
                    $this->deleteVersionedObject(\@$localName, $local);
                    delete $synch->{local}{$lsname};
                    delete $synch->{remote}{$rsname};
                    _writeDB($synch);
                    $reporter->report("Removed $act", 1);
                } otherwise {
                    my $e = shift;
                    my $mess = $e->{-text} || '';
                    #print STDERR "RWOP2: $e->{-file}:$e->{-line}\n";
                    $reporter->report("Error while synching $lsname: ".
                                        $mess, 2);
                    $this->logEvent("** Error while deleting $act: $mess");
                };
            }
            else {
                $reporter->report(
                    'Did NOT remove '.$this->{localID}.':'.
                      $lsname.'@'.$local.' (gone on '.
                        $this->{remoteID}.
                          ' and not updated locally) because receiving changes is inhibited', 1);
            }
        }
        # Otherwise the topic is new locally, or has been deleted on
        # the remote but updated locally
        else {
            if ($this->{transmit}) {
                my $act = "$this->{localID}:$lsname";
                $act .= "\@$synch->{local}{$lsname}"
                  if defined $synch->{local}{$lsname};
                $act .= " to $this->{remoteID}";
                $this->logEvent("Uploading $act");
                try {
                    $synch->{remote}{$rsname} = $this->updateRemoteFromLocal(
                        $localName, $remoteName, $remote);
                    $synch->{local}{$lsname} = $local;
                    _writeDB($synch);
                    $reporter->report(
                        'Uploaded '.$this->{localID}.':'.
                          $lsname.'@'.$synch->{local}{$lsname}.
                            ' to '.$this->{remoteID}.':'.
                              $rsname.'@'.$synch->{remote}{$rsname}, 1);
                } otherwise {
                    my $e = shift;
                    my $mess = $e->{-text} || '';
                    #print STDERR "RWOP3: $e->{-file}:$e->{-line}\n";
                    $reporter->report("Error while synching $lsname: ".
                                        $mess, 2);
                    # Don't need to flag an error, because the local version
                    # is the one we want to keep. Log it, though.
                    $this->logEvent("** Error while sending $act: $mess");
                };
            }
            else {
                $reporter->report(
                    'Did NOT upload '.$this->{localID}.':'.
                      $lsname.'@'.$synch->{local}{$lsname}.
                        ' to '.$this->{remoteID}.':'.
                          $rsname.'@'.$synch->{remote}{$rsname}.
                            " because sending changes is inhibited", 1);
            }
        }
        return;
    }

    # New topics that only exist on one side have been dealt with.
    # Now examine individual topics.

    # If a topic has never been synched, we have to assume the
    # original rev was 0
    unless (defined $synch->{local}{$lsname}) {
        $synch->{local}{$lsname} = 0;
    }
    unless (defined $synch->{remote}{$rsname}) {
        $synch->{remote}{$rsname} = 0;
    }
    if ($local != $synch->{local}{$lsname}) {
        # Topic is changed locally since the last synch. Is it mergeable?
        if ($merge && $remote != $synch->{remote}{$rsname}) {
            # Topic is also changed remotely since the last synch
            if ($this->{receive} and $this->{transmit}) {
                my $act = "$lsname\@$local with "
                  . "$this->{remoteID}:$rsname\@$remote";
                $this->logEvent("Merging $act");
                try {
                    my ($r, $l) = $this->mergeRemoteAndLocal(
                        $localName, $synch->{local}{$lsname}, $local,
                        $remoteName, $synch->{remote}{$rsname}, $remote);
                    $synch->{remote}{$rsname} = $r;
                    $synch->{local}{$lsname} = $l;
                    _writeDB($synch);
                    $reporter->report("Merged $act", 1);
                } otherwise {
                    my $e = shift;
                    my $mess = $e->{-text};
                    #print STDERR "RWOP4: $e->{-file}:$e->{-line}\n";
                    $reporter->report("Error while synching $lsname: ".
                                        $mess, 2);
                    $this->logEvent("** Error while merging $act: $mess");
                };
            }
            else {
                $reporter->report(
                    'Did NOT merge '.$this->{localID}.':'.$lsname.'@'.$local.
                      ' with '.$this->{remoteID}.':'.$rsname.'@'.$remote.
                        " because sending and/or receiving of changes is inhibited", 1);
            }
        }
        else {
            # Topic is changed locally, but not on remote, or it isn't
            # mergeable
            if ($this->{transmit}) {
                my $act = "$this->{remoteID}:$rsname from $lsname\@$local";
                $this->logEvent('Updating ', $act);
                try {
                    $synch->{remote}{$rsname} = $this->updateRemoteFromLocal(
                        $localName, $remoteName, $remote);
                    $synch->{local}{$lsname} = $local;
                    _writeDB($synch);
                    $reporter->report(
                        'Updated '.$this->{remoteID}.':'.
                          $rsname.'@'.$synch->{remote}{$rsname}.
                            ' from '.$this->{localID}.':'.$lsname
                              .'@'.$local, 1);
                } otherwise {
                    my $e = shift;
                    my $mess = $e->{-text} || '';
                    #print STDERR "RWOP5: $e->{-file}:$e->{-line}\n";
                    $reporter->report("Error while synching $lsname: ".
                                        $mess, 2);
                    $this->logEvent("** Error while updating $act: $mess");
                    undef $@;
                };
            }
            else {
                $reporter->report(
                    'Did NOT update '.$this->{remoteID}.':'.
                      $rsname.'@'.$synch->{remote}{$rsname}.
                        ' from '.$this->{localID}.':'.$lsname.'@'.$local.
                          " because sending changes is inhibited", 1);
            }
        }
        return;
    }

    # Topic is not changed locally

    if ($remote != $synch->{remote}{$rsname}) {
        if ($this->{receive}) {
            my $act = "$lsname from $this->{remoteID}:$rsname\@$remote";
            $this->logEvent('Getting new ', $act);
            try {
                # Topic is changed remotely
                $synch->{local}{$lsname} =
                  $this->updateLocalFromRemote(
                      $localName, $local, $remoteName);
                $synch->{remote}{$rsname} = $remote;
                _writeDB($synch);
                $reporter->report(
                    'Got new '.
                      $this->{localID}.':'.$lsname
                        .'@'.$synch->{local}{$lsname}.
                        ' from '.$this->{remoteID}.':'.$rsname.'@'.$remote, 1);
            } otherwise {
                my $e = shift;
                my $mess = $e->{-text};
                #print STDERR "RWOP6: $e->{-file}:$e->{-line}\n";
                $reporter->report("Error while synching $lsname: ".
                                    $mess, 2);
                $this->logEvent("** Error while receiving $act: $mess");
            };
        }
        else {
            $reporter->report(
                'Did NOT get new '.
                  $this->{localID}.':'.$lsname.'@'.$synch->{local}{$lsname}.
                    ' from '.$this->{remoteID}.':'.$rsname.'@'.$remote.
                      " because receiving changes is inhibited", 1);
        }

        return;
    }

    # Topic is not changed remotely or locally - at least, they are
    # still at the same revision that they were at after the last
    # synch.
}

# Get a handler for the given store object
# $this->getStore( $web [, $topic [, $attachment]])
sub getStore {
    my $this = shift;
    eval "use $this->{handlerClass} ()";
    die $@ if $@;
    return $this->{handlerClass}->new($this->{session}, @_);
}

# Add local rev to the remote.
# Return the final remote rev.
sub updateRemoteFromLocal {
    my ($this, $localName, $remoteName, $rev) = @_;
    ASSERT(ref($localName) eq 'ARRAY');
    ASSERT(ref($remoteName) eq 'ARRAY');
    my $handler = $this->getStore(@$localName);

    # Get the local revision
    my $text = $handler->getRevision(0);

    # Post it up to the remote
    my $rrev = $this->putToRemote($remoteName, $text, $rev);

    return $rrev;
}

# Add remote rev to the local topic, which must be rev $rev. Return the
# final local rev.
# $web = web name
# $topic = topic name
# $attachment = attachment name
sub updateLocalFromRemote {
    my ($this, $localName, $rev, $remoteName) = @_;
    ASSERT(ref($localName) eq 'ARRAY');
    ASSERT(ref($remoteName) eq 'ARRAY');

    my $text = $this->getFromRemote($remoteName, undef);

    # Save the local revision
    my $handler = $this->getStore(@$localName);
    # Check that the current rev is the expected rev, or abort the synch
    die unless $rev eq $handler->currentRevision();
    return $handler->addRevision($text);
}

sub _verifyResponse {
    my ($this, $op, $response) = @_;
    unless ($response->is_success ) {
        throw Error::Simple(
            "$op: failed ".$response->request->uri().
              ' -- '.$response->status_line."\n".$response->content());
    }
    my $contents = $response->content();
    my $ver = $this->{verify}++;
    if( $contents =~ s/^$ver (\d+)\r?\n//s ) {
        my $checksum = $1;
        my $nowsum = unpack("%32C*", $contents) % 65535;
        unless ($checksum == $nowsum) {
            die "$op: Message verification failed "
              .$response->request->uri()
                .' -- incorrect checksum; expected '
                  ."$checksum, received $nowsum\n'"
                    .$response->content()."'";
        }
    } else {
        die "$op: Message verification failed "
          .$response->request->uri().
            " -- invalid response verification; expected $ver, received\n"
              .$response->content();
    }

    $this->{serverTime} = $response->date();
    return $contents;
}

# Get a rev from the remote, using the REST interface
sub getFromRemote {
    my ($this, $name, $rev) = @_;
    ASSERT(ref($name) eq 'ARRAY');
    my %form = (
        _web => $name->[0],
        _topic => $name->[1],
        _attachment => $name->[2],
        _rev => $rev,
        _requester => $this->{localID},
        username => $this->{username},
        password => $this->{password},
        verification => $this->{verify},
       );
    my $url = $this->{restURL}.'get';
    my $response = $this->{userAgent}->post($url, \%form);
    return $this->_verifyResponse('GET', $response);
}

# Put a rev on the remote, using the REST interface
sub putToRemote {
    my ($this, $name, $text, $rev) = @_;
    ASSERT(ref($name) eq 'ARRAY');
    ASSERT(defined($rev));
    my %form = (
        _web => $name->[0],
        _topic => $name->[1],
        _attachment => $name->[2],
        _text => $text,
        _comment => 'Sent from '.$this->{localID},
        _rev => $rev,
        _requester => $this->{localID},
        username => $this->{username},
        password => $this->{password},
        verification => $this->{verify},
       );
    my $url = $this->{restURL}.'put';
    my $response = $this->{userAgent}->post($url, \%form);
    return $this->_verifyResponse('PUT', $response);
}

# Merge remote and local changes. Update both remote and local with the
# changes. Return the new rev numbers.
# $localName = array containing local name components
# (web, topic, attachment)
# $oLrev = last local rev that was synched
# $cLrev = current local rev
# $remoteName = array containing remote name components
# (web, topic, attachment)
# $oRrev = last remote rev that was synched
# $cRrev = current remote rev
# Returns ($remoteRev, $localRev), the remote and local rev numbers after
# the merge.
sub mergeRemoteAndLocal {
    my ($this,
        $localName, $oLrev, $cLrev,
        $remoteName, $oRrev, $cRrev) = @_;
    ASSERT(ref($localName) eq 'ARRAY');
    ASSERT(ref($remoteName) eq 'ARRAY');
    my $session = $this->{session};

    my $handler = $this->getStore(@$localName);

    # Get the common ancestor revision
    my $common = $handler->getRevision($oLrev);

    # Strip out the meta-data and throw it away
    my ($lweb, $ltopic) = @$localName;
    my $meta = new Foswiki::Meta($session, $lweb, $ltopic);
    $session->{store}->extractMetaData($meta, \$common);

    # Get the latest local revision
    my $local = $handler->getRevision($cLrev);
    my $lmeta = new Foswiki::Meta($session, $lweb, $ltopic);
    $session->{store}->extractMetaData($lmeta, \$local);

    # Get the latest revision from the remote
    my ($rweb, $rtopic) = @$remoteName;
    my $remote = $this->getFromRemote($remoteName, $cRrev);
    my $rmeta = new Foswiki::Meta($session, $rweb, $rtopic);
    $session->{store}->extractMetaData($rmeta, \$remote);

    # Merge the common ancestor with the two derivatives
    my $new = Foswiki::Merge::merge3(
        "$oRrev($this->{remoteID})==$oLrev($this->{localID})", $common,
        "$cRrev ($this->{remoteID})", $remote,
        "$cLrev ( $this->{localID})", $local,
        '.*?\n', $session );

    # Merge the form, if any
    my $formName = $lmeta->getFormName();
    if ($formName) {
        my $formDef = new Foswiki::Form( $session, $lweb, $formName );
        if ($formDef) {
            $lmeta->merge($rmeta, $formDef);
        }
    } else {
        # Otherwise copy the form from remote, if any.
        $formName = $rmeta->getFormName();
        if ($formName) {
            $lmeta->put('FORM', { name => $formName });
            my $data = $rmeta->{FIELD};
            foreach my $fld ( @$data ) {
                $lmeta->putKeyed('FIELD', $fld );
            }
        }
    }

    my $nLrev = $cLrev + 1;
    my %options =
      (
          version => '1.'.$nLrev,
          date    => time(),
          author  => Foswiki::Func::getWikiName(),
          format  => $Foswiki::Store::STORE_FORMAT_VERSION,
         );
    $lmeta->put('TOPICINFO', \%options);

    if ($lmeta->can('getEmbeddedStoreForm')) {
        $lmeta->text($new);
        $local = $lmeta->getEmbeddedStoreForm();
    } else {
        $local = Foswiki::Store::_writeMeta($lmeta, $new);
    }
    $handler->addRevision($local);

    my $nRrev = $cRrev + 1;
    $options{version} = '1.'.$nRrev;
    $lmeta->put('TOPICINFO', \%options);

    if ($lmeta->can('getEmbeddedStoreForm')) {
        $remote = $lmeta->getEmbeddedStoreForm();
    } else {
        $remote = Foswiki::Store::_writeMeta($lmeta, $new);
    }
    $cRrev = $this->putToRemote($remoteName, $remote, $cRrev);
    # Check that the actual new remote rev is consistent with the expected
    # new remote rev
    die "Synchronisation failure $nRrev $cRrev"
      unless $cRrev == $nRrev;

    return ($nRrev, $nLrev);
}

# Delete a remote topic or attachment
sub deleteVersionedObjectFromRemote {
    my ($this, $name, $rev) = @_;
    my ($web, $topic, $attachment) = @$name;
    ASSERT(defined $rev);
    my %form = (
        _web => $web,
        _topic => $topic,
        _attachment => $attachment,
        _rev => $rev,
        _requester => $this->{localID},
        verification => $this->{verify},
       );
    my $url = $this->{restURL}.'dvo';
    my $response = $this->{userAgent}->post($url, \%form);
    $this->_verifyResponse('Delete Versioned Object', $response );
}

# Delete a local topic or attachment
sub deleteVersionedObject {
    my ($this, $name, $rev) = @_;
    ASSERT(ref($name) eq 'ARRAY');
    ASSERT(defined $rev);
    my $handler = $this->getStore(@$name);
    my $curr = $handler->currentRevision();
    # Commented out because it should never happen
    #return unless $curr; # does the object exist (may already have been done)
    die join('.', map {$_||''} @$name).
      " was updated ($curr > $rev) - cannot delete"
        unless $curr == $rev;
    $handler->remove();
}

# Call getEnv on the remote node
sub getEnvFromRemote {
    my $this = shift;

    my %form = (
        _requester => $this->{localID},
        username => $this->{username},
        password => $this->{password},
        verification => $this->{verify},
       );
    my $url = $this->{restURL}.'info';
    my $response = $this->{userAgent}->post($url, \%form);
    my $content = $this->_verifyResponse('INFO', $response);
    my %info;
    foreach my $kvp (split/\r?\n/, $content) {
        die $content unless ($kvp =~ /^(.*?)=(.*)$/);
        $info{$1} = Foswiki::urlDecode($2);
    }
    return \%info;
}

# Get a hash mapping key-value pairs of useful info
sub getEnv {
    my $this = shift;
    my %info = ( TIME => time() );
    foreach my $key qw(Foswiki::VERSION
                       Foswiki::RELEASE
                       Foswiki::Plugins::VERSION
                       Foswiki::Plugins::ReadWriteOfflinePlugin::VERSION
                       Foswiki::Plugins::ReadWriteOfflinePlugin::RELEASE) {
        $info{$key} = eval '$'.$key;
    }
    return \%info;
}

# Call getContents on the remote node
sub getContentsFromRemote {
    my ($this, $web, $topic) = @_;

    my %form = (
        _requester => $this->{localID},
        _web => $web,
        _topic => $topic,
        verification => $this->{verify},
       );
    my $url = $this->{restURL}.'contents';
    my $response = $this->{userAgent}->post($url, \%form);
    my $text = $this->_verifyResponse('getContentsFromRemote', $response );
    my %contents;
    foreach my $content (split/\r?\n/, $text) {
        die $content unless ($content =~ /^(.*)=(\S+)$/);
        $contents{$1} = $2;
    }
    return \%contents;
}

# Get a hash mapping web content types and names to revs,
# for the *current* install. The hash returned contains
# three types of key, indicated by W!, T! or A! prepended to
# the key name. W! indicates a web, T! a topic a A! an attachment.
# The same keys with timestamp_ prepended gives the revision timestamp.
# A rev of -1 indicates an error during the last synch of this object.
sub getContents {
    my ($this, $web) = @_;
    ASSERT($this->isa('Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher'));

    # Ticklish.
    #   1 Foswiki doesn't keep any reliable record of a move in the
    #     topic meta-data (TOPICMOVED is only added for topics moved using
    #     the UI).
    #   2 A topic move is a simple rename, which doesn't touch the topic
    #     and even if it did, most stores use the checkin date rather
    #     than the file time
    # Because of this we need to sniff the log for any 'rename' events that
    # resulted in a new topic. This isn't cast-iron because the log files
    # are rotated by date, and we only process logfiles up to a month old.
    # Older renames will be missed.

    # Process the log for 'rename' events, and if any result in a destination
    # topic in the contents, then update all associated attachment dates as
    # well
    my $log = $Foswiki::cfg{LogFileName};
    my $when = time();
    my $lastLogYear = Foswiki::Time::formatTime($when, '$year', 'servertime');
    my $lastLogMonth = Foswiki::Time::formatTime($when, '$mo', 'servertime');

    my $logYear = $lastLogYear;
    my $logMonth = $lastLogMonth;
    if ($log =~ /%DATE%/) {
        my $then = $when - 31 * 24 * 60 * 60; # one month
        $logYear = Foswiki::Time::formatTime($then, '$year', 'servertime');
        $logMonth = Foswiki::Time::formatTime($then, '$mo', 'servertime');
    }

    my %movetime;
    while (1) {
        $log = $Foswiki::cfg{LogFileName};
        my $time = $logYear.sprintf("%02d", $logMonth);
        $log =~ s/%DATE%/$time/g;
        my $f;
        local $/ = "\n";
        if (open($f, '<', $log)) {
            while (<$f>) {
                if (/\| ([^|]+) \| [^|]* \| rename \| [^|]+ \| moved to $web\.([^|.]*) \|/) {
                    my ($d, $tt) = ($1, $2);
                    $d = Foswiki::Time::parseTime($d);
                    # Foswiki log timestamps the nearest minute, so we have to
                    # take the *end* of that minute as a worst-case time for
                    # the rename; or the current time, if earlier.
                    if (($d||0) > ($movetime{$tt}||0)) {
                        $movetime{$tt} = List::Util::min($d + 60, $when);
                    }
                }
            }
            close($f);
        }
        last if $logMonth == $lastLogMonth && $logYear == $lastLogYear;
        $logMonth++;
        if ($logMonth == 13) {
            $logMonth = 1;
            $logYear++;
        }
    }

    my %contents;
    my $wh = $this->getStore($web);
    my $tit = $wh->eachTopic();
    while ($tit->hasNext()) {
        my $topic = $tit->next();
        $movetime{$topic} ||= 0;
        my $th = $this->getStore($web, $topic);
        my ($trev, $tdate) = $th->getInfo();
        if ($movetime{$topic} > $tdate) {
            $tdate = $movetime{$topic};
        }
        $contents{"T!$topic"} = $trev;
        $contents{"timestamp_T!$topic"} = $tdate;
        my $ait = $th->eachAttachment();
        while ($ait->hasNext()) {
            my $att = $ait->next();
            my $ah = $this->getStore($web, $topic, $att);
            my ($arev, $adate) = $ah->getInfo();
            if ($movetime{$topic} > $adate) {
                $adate = $movetime{$topic};
            }
            $contents{"A!$topic.$att"} = $arev;
            $contents{"timestamp_A!$topic.$att"} = $adate;
        }
    }
    my $swit = $wh->eachSubWeb();
    while ($swit->hasNext()) {
        my $subweb = $swit->next();
        # No version information for webs
        $contents{"W!$subweb"} = 1;
    }

    return \%contents;
}

# Call test on the remote node
sub testOnRemote {
    my ($this, $action) = @_;

    my $url = $this->{restURL}.'test?role=passive;action='.$action.';rid='.
      $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{LocalID};

    my $response = $this->{userAgent}->get($url);
    unless ($response->is_success ) {
        die "Failed to get info; ".$response->request->uri().
          ' -- '.$response->status_line."\n".$response->content();
    }
    my $remote = $response->content();
    die "Failed: ".($remote||'undef') unless $remote && $remote eq 'OK';
}

1;
