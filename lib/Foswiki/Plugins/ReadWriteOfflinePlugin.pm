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
# This code cost a lot in blood, sweat and tears to develop, and you
# are respectfully requested to purchase a support contract from
# http://c-dot.co.uk. By doing so you are helping to make the
# further development of open-source Foswiki possible.
#
# Support for synching a remote Foswiki with this Foswiki
# (both must be using a Store that implements the rcsFile protocol).
#
# This package implements both the server and client sides of the
# arrangement. The server provides REST entry points to support the
# client querying it for data. The client has one REST entry point,
# 'synch', which is what triggers a synch process.
#
# The client has to be able to authorise on the server to be able to
# synch completely. The plugin will refuse to synch any files to which
# it doesn't have change access.
#

package Foswiki::Plugins::ReadWriteOfflinePlugin;

use strict;
use Error qw(:try);

use Foswiki ();
use Foswiki::Plugins ();
use Foswiki::Sandbox ();
use Foswiki::Func ();

# %$TRACKINGCODE%

our $VERSION = '$Rev: 956 (22 Feb 2008) $';
our $RELEASE = '24 Jul 2009';
our $SHORTDESCRIPTION = <<DESC;
Supports offline reading and writing in a second wiki
DESC
our $NO_PREFS_IN_TOPIC = 1;

# Cache of the status, so we can get it from the REST result 
our $pendingStatus;

our $trashWeb =
  $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{Trash} ||
  $Foswiki::cfg{TrashWebName} || 'Trash';

sub initPlugin {
    #my ($topic, $web, $user, $installWeb) = @_;

    Foswiki::Func::registerRESTHandler('info', \&_rest_infoHandler);
    Foswiki::Func::registerRESTHandler('get', \&_rest_getHandler);
    Foswiki::Func::registerRESTHandler('put', \&_rest_putHandler);
    Foswiki::Func::registerRESTHandler('contents', \&_rest_contentsHandler);
    Foswiki::Func::registerRESTHandler('dvo', \&_rest_dvoHandler);
    Foswiki::Func::registerRESTHandler('synch', \&_rest_synchHandler);
    Foswiki::Func::registerRESTHandler('test', \&_rest_testHandler);

    Foswiki::Func::registerTagHandler('READWRITEOFFLINE_RIDS', \&ridList);

    return 1;
}

# implant the pending status in the header. This handler will be invoked
# from Foswiki::Func::writeHeader.
sub modifyHeaderHandler {
    my $headers = shift;
    $headers->{status} = $pendingStatus;
    # Add the time so that the client can be kept in synch with the server
    $headers->{date} = time();
}

# Support for REST handlers. Generate a status code and message output.
# Returns undef, so can be used with return in a REST handler.
sub _REST_respond {
    my ( $response, $status, $text ) = @_;

    if ($status < 400 && defined $query->param('verification')) {
        my $checksum = unpack("%32C*", $text) % 65535;
        $text = $query->param('verification')." $checksum\r\n$text";
    }

    # Foswiki 1.0 introduces the Foswiki::Response object, which handles all
    # responses.
    if ( UNIVERSAL::isa( $response, 'Foswiki::Response' ) ) {
        $response->header(
            -status  => $status,
            -type    => 'text/plain',
            -charset => 'UTF-8'
        );
        $response->print($text);
    }
    else {
        # Pre-Foswiki-1.0.
        # Turn off AUTOFLUSH
        # See http://perl.apache.org/docs/2.0/user/coding/coding.html
        local $| = 0;
        my $query = Foswiki::Func::getCgiQuery();
        if ( defined($query) ) {
            my $len;
            { use bytes; $len = length($text); };
            print $query->header(
                -status         => $status,
                -type           => 'text/plain',
                -charset        => 'UTF-8',
                -Content_length => $len
            );
            print $text;
        }
    }
    print STDERR $text if ( $status >= 400 );
}

# Support for REST handlers
sub _getTarget {
    my $response = shift;

    my $query = Foswiki::Func::getCgiQuery();

    my $web = $query->param('_web') || '';
    my $topic = $query->param('_topic') || '';
    my $attachment = $query->param('_attachment') || '';
    if (!$web || $web =~ /$Foswiki::cfg{NameFilter}/) {
        _REST_respond($response, 400, "Illegal web name $web");
        return ();
    }
    if ($topic && $topic =~ /$Foswiki::cfg{NameFilter}/) {
        _REST_respond($response, 400, "Illegal topic name $topic");
        return ();
    }
    if ($attachment && $attachment =~ /$Foswiki::cfg{UploadFilter}/) {
        _REST_respond($response, 400, "Illegal attachment name $attachment");
        return ();
    }
    $web = Foswiki::Sandbox::untaintUnchecked($web);
    $topic = Foswiki::Sandbox::untaintUnchecked($topic);
    $attachment = Foswiki::Sandbox::untaintUnchecked($attachment);
    return ($web, $topic, $attachment);
}

=begin TML

---+++ REST handler =ReadWriteOfflinePlugin/info=
URL parameters: none

Responds with a list of key-value pairs. The server will
at least respond with:
| *Key* | *Meaning* |
| =Foswiki::VERSION= | $VERSION in Foswiki.pm |
| =Foswiki::RELEASE= | $RELEASE from Foswiki.pm |
| =Foswiki::Plugins::ReadWriteOfflinePlugin::VERSION= | $VERSION from ReadWriteOfflinePlugin.pm |
| =Foswiki::Plugins::ReadWriteOfflinePlugin::RELEASE= | $RELEASE from ReadWriteOfflinePlugin.pm |
The response is a newline-separated list of key-value pairs. The format is
=key=value=. The value will be URL-encoded.

=cut

sub _rest_infoHandler {
    my ( $session, $plugin, $verb, $response ) = @_;
    my $reply = '';

    eval {
        require Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher;
    };
    return _REST_respond($response, 500, $@) if $@;

    my $syncher = new Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher(
        $Foswiki::Plugins::SESSION,
        $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{LocalID});

    my $info = $syncher->getEnv();
    while (my ($key, $value) = each %$info) {
        $reply .= $key.'='.Foswiki::urlEncode($value||'')."\n";
    }
    $syncher->finish();
    return _REST_respond($response, 200, $reply);
}

=begin TML

---+++ REST handler =ReadWriteOfflinePlugin/dvo=
URL parameters:
   * =_web= - web name (required)
   * =_topic= - topic (required)
   * =_attachment= - attachment name (optional)
Deletes the given versioned object.

=cut

sub _rest_dvoHandler {
    my ( $session, $plugin, $verb, $response ) = @_;

    eval {
        require Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher;
    };
    return _REST_respond($response, 500, $@) if $@;

    my $syncher = new Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher(
        $Foswiki::Plugins::SESSION,
        $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{LocalID});

    my $query = Foswiki::Func::getCgiQuery();
    my $rev = $query->param('_rev');
    my $requester = $query->param('_requester');

    my @name = _getTarget($response);
    return undef unless scalar(@name);

    try {
        $syncher->logEvent('Removing ', \@name, ' because ',
                           $requester, ' requested it');
        $syncher->deleteVersionedObject(\@name, $rev);
        $syncher->finish();
        _REST_respond($response, 200, '');
    } otherwise {
        my $mess = shift->{-text};
        $syncher->logEvent('** Error removing ', \@name, ": $mess");
        $syncher->finish();
        _REST_respond($response, 500, $mess);
    };
    return undef;
}

=begin TML

---+++ REST handler =ReadWriteOfflinePlugin/get=
URL parameters:
   * =_web= - web name (required)
   * =_topic= - topic (required)
   * =_attachment= - attachment name (optional)
   * =_rev= - rev to fetch (optional)
Returns the full plain text of the given rev of the given topic or attachment.
By default fetches the most recent rev.

=cut

sub _rest_getHandler {
    my ( $session, $plugin, $verb, $response ) = @_;

    my ($web, $topic, $attachment) = _getTarget($response);
    return undef unless $web;

    my $query = Foswiki::Func::getCgiQuery();
    my $rev = $query->param('_rev');

    eval {
        require Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher;
    };
    return _REST_respond($response, 500, $@) if $@;

    my $syncher = new Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher(
        $Foswiki::Plugins::SESSION,
        $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{LocalID});

    my $handler = $syncher->getStore($web, $topic, $attachment);
    my $topRev = $handler->currentRevision();
    $rev ||= $topRev;
    if ($rev < 1 || $rev > $topRev) {
        return _REST_respond($response, 500,
                     "Non-existant rev $web/$topic/$attachment\@$rev "
                       . "(max rev is $topRev)");
    }
    my $text;

    try {
        $text = $handler->getRevision($rev);
        $syncher->finish();
        unless (Foswiki::Func::checkAccessPermission(
            'view', Foswiki::Func::getWikiName(), $text, $topic, $web)) {
            _REST_respond($response, 403, "Access control violation $web $topic");
        }
        _REST_respond($response, 200, $text);
    } otherwise {
        my $mess = shift->{-text};
        $syncher->logEvent('** Error getting ', [ $web, $topic, $attachment ],
                           ': ', $mess);
        $syncher->finish();
        _REST_respond($response, 500, $mess);
    };
    return undef;
}

=begin TML

---+++ REST handler =ReadWriteOfflinePlugin/put=
URL parameters:
   * =_web= - web name (required)
   * =_topic= - topic (required)
   * =_attachment= - attachment name (optional)
   * =_text= - new text of the topic/attachment (optional)
   * =_comment= - checkin comment (optional)
Checks in a new revision of the topic/attachment using the given text as the
content. Returns the number of the new revision.

=cut

sub _rest_putHandler {
    my ( $session, $plugin, $verb, $response ) = @_;

    my ($web, $topic, $attachment) = _getTarget($response);
    return undef unless $web;

    my $query = Foswiki::Func::getCgiQuery();
    my $text = $query->param('_text');
    my $rev = $query->param('_rev');
    my $comment = $query->param('_comment') || '';
    my $requester = $query->param('_requester') || '';

    # Make sure web, topic and attachment are all valid
    return _REST_respond($response, 400, "$web is the trash web") if ($web eq $trashWeb/);
    return _REST_respond($response, 400, "$web is an illegal web name")
      if ($web =~ /$Foswiki::cfg{NameFilter}/);
    return _REST_respond($response, 400, "$topic is an illegal topic name")
      if ($topic =~ /$Foswiki::cfg{NameFilter}/);
    return _REST_respond($response, 400, "$attachment is an illegal attachment name")
      if ($attachment =~ /$Foswiki::cfg{UploadFilter}/
            || $attachment =~ /$Foswiki::cfg{NameFilter}/);

    eval {
        require Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher;
    };
    return _REST_respond($response, 500, $@) if $@;

    my $syncher = new Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher(
        $Foswiki::Plugins::SESSION,
        $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{LocalID});

    my $handler = $syncher->getStore($web, $topic, $attachment);

    unless (Foswiki::Func::checkAccessPermission(
        'change', Foswiki::Func::getWikiName(), undef, $topic, $web )) {
        return _REST_respond($response, 403, "Access control violation $web $topic");
    }
    # Check that the current rev is the expected rev
    if ($handler->currentRevision() != $rev) {
        return _REST_respond($response, 500, "$web.$topic has been modified since the synch started. Expected rev $rev, but is rev ".$handler->currentRevision());
    }
    my $newRev;
    try {
        $syncher->logEvent(
            'Updating ', [ $web, $topic, $attachment ],
            ' with new version from ', $requester);
        $newRev = $handler->addRevision($text);
        $syncher->finish();
        _REST_respond($response, 200, $newRev);
    } otherwise {
        my $mess = shift->{-text};
        $syncher->logEvent(
            '** Error updating ', [ $web, $topic, $attachment ], ": $mess");
        $syncher->finish();
        _REST_respond($response, 500, $mess);
    };
    return undef;
}

=begin TML

---+++ REST handler =ReadWriteOfflinePlugin/contents=
URL parameters:
   * =_web= - web name (required)
Gets the contents of a web in the server. The list is returned as a
newline-separated list of object identifiers. An object identifier is a
formatted string:

_type_ ! _path_ = _rev_

Where type is one of =W= (subweb), =T= (topic) or =A= (attachment). Attachment names include the containing topic. For example,

<verbatim>
T!WebPreferences=6
T!WebHome=4
T!PictureBook=7
A!PictureBook.pussy.gif=3
A!PictureBook.doggy.gif=5
W!Stories=0
</verbatim>

=cut

sub _rest_contentsHandler {
    my ( $session, $plugin, $verb, $response ) = @_;

    my ($web, $topic) = _getTarget($response);
    return undef unless $web;

    $web = Foswiki::Sandbox::untaintUnchecked($web);

    return _REST_respond($response, 400, "Illegal web name") unless $web;

    unless (Foswiki::Func::checkAccessPermission(
        'view', Foswiki::Func::getWikiName(), undef,
        $Foswiki::cfg{WebPrefsTopicName}, $web )) {
        return _REST_respond($response, 403, "Access control violation");
    }

    eval {
        require Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher;
    };
    return _REST_respond($response, 500, $@) if $@;

    my $syncher = new Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher(
        $Foswiki::Plugins::SESSION,
        $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{LocalID});

    my $list = $syncher->getContents($web);
    $syncher->finish();

    return stop(200, join("\n", map { "$_=$list->{$_}" } keys %$list));
}

{
    package Reporter;

    sub new {
        my ($class) = @_;
        my $html;
        if (!$Foswiki::Plugins::SESSION ||
              $Foswiki::Plugins::SESSION->inContext('command_line')) {
            $html = 0;
        }
        else {
            $html = 1;
        }
        return bless({ html => $html, anchor_count => undef }, $class);
    }

    sub report {
        my $this = shift;
        my $message = shift;
        my $status = shift || 0;
        if (!$this->{html}) {
            print "$message\n";
        } else {
            if (not defined $this->{anchor_count})
            {
                $this->{anchor_count} = 1;
                print CGI::a({-href=>"#anchor".$this->{anchor_count}},"Click here to jump to first change").CGI::br();
            }
            $message .= CGI::br();
            if ($status == 2)
            {
                print CGI::strong($message);
            }
            elsif ($status == 1)
            {
                print CGI::a({-name=>"anchor".$this->{anchor_count}},'');
                $this->{anchor_count}++;
                print CGI::a({-href=>"#anchor".$this->{anchor_count}},"Next").' ';
                print CGI::strong($message);
            }
            else
            {
                print $message;
            }
        }
    }
};

=begin TML

---+++ REST handler =ReadWriteOfflinePlugin/synch=
URL parameters:
   * =webs= - comma-separated list of web names (required)
   * =rid= - remote ID of server to synch with
   * =reset= - if set, then the plugin will abandon any caches and assume
     the webs have never been synched with this server before. This will
     result in new common versions on both server and client.
   * =direction= - 'rx' for receive-only, 'tx' for transmit-only,
     or 'rxtx' for both. Defaults to 'rxtx';

Invoked on the _client_ (*not* the _server_) to synchronise a number of webs
with the identified server. This handler is usally invoked from a form, or
via the command-line.

=cut

sub _rest_synchHandler {
    my ( $session, $plugin, $verb, $response ) = @_;

    my $query = Foswiki::Func::getCgiQuery();
    my $webs = $query->param('webs') || '';
    my $rid = $query->param('rid');
    my $reset = $query->param('reset');
    my $direction = $query->param('direction');

    unless ($webs) {
        return _REST_respond($response, 400, "Must give a list of webs to synch");
    }
    $webs = Foswiki::Sandbox::untaintUnchecked($webs);

    eval {
        require Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher;
    };
    return _REST_respond($response, 500, $@) if $@;

    my $syncher = new Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher(
        $Foswiki::Plugins::SESSION,
        $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{LocalID},
        $direction);

    my $reporter = new Reporter();
    unless ($Foswiki::Plugins::SESSION->inContext('command_line')) {
        print CGI::header('text/html');
        print CGI::title('Synch report');
        print CGI::start_html();
    }
    if ($syncher->setRemoteServer($rid, $reporter)) {
        foreach my $web (split(/\s*,\s*/, $webs)) {
            my $rweb;
            if ($web =~ /^(.+)=(.+)$/) {
                $rweb = $2;
                $web = $1;
            } elsif ($web =~ /^(.*)$/) {
                # Untainted
                $rweb = $1;
            }

            if ($web =~ /$Foswiki::cfg{NameFilter}/) {
                $reporter->report("Ignoring illegal web name $web", 2);
            } elsif ($rweb =~ /$Foswiki::cfg{NameFilter}/) {
                $reporter->report("Ignoring illegal web name $rweb", 2);
            } elsif ( $web eq $trashWeb ) {
                $reporter->report("Ignoring trash web $web", 2);
            } else {
                $syncher->synchWeb($web, $rweb, $reporter, $reset);
            }
        }
    }
    $syncher->finish();

    unless ($Foswiki::Plugins::SESSION->inContext('command_line')) {
        print CGI::end_html();
    }
    return undef;
}

# REST entry point for testing.
sub _rest_testHandler {
    my ( $session, $plugin, $verb, $response ) = @_;

    my $query = Foswiki::Func::getCgiQuery();
    my $rid = $query->param('rid') || 'loopback';
    my $role = $query->param('role') || 'active';
    my $action = $query->param('action') || '';
    eval {
        require Foswiki::Plugins::ReadWriteOfflinePlugin::Test;
        Foswiki::Plugins::ReadWriteOfflinePlugin::Test::run(
            $role, $rid, $action);
    };
    _REST_respond($response, 500, $@) if $@
    return undef;
}

# Tag handler for READWRITEOFFLINE_RIDS
sub ridList {
    my($session, $params, $topic, $web) = @_;
    my $format = $params->{format};
    $format = '$rid' unless defined $format;
    my $sep = $params->{separator};
    $sep = ',' unless defined $sep;

    my $text = '';
    my $servers = $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{Servers};
    foreach my $rid (keys(%$servers)) {
        my $server =
          $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{Servers}->{$rid};
        next unless ref( $server ) eq 'HASH';
        my $row = $format;
        $row =~ s/\$rid/$rid/g;
        $row =~ s/\$duh/$server->{DefaultUrlHost}/g;
        $row =~ s/\$sup/$server->{ScriptUrlPath}/g;
        $row =~ s/\$ss/$server->{ScriptSuffix}/g;
        $text .= $row;
    }
    $text = Foswiki::Func::decodeFormatTokens($text);
    return $text;
}

1;
