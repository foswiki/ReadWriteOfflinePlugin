#
# Copyright (C) 2006-2008 Crawford Currie http://wikiring.com
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

# Interface to the standard Store, via Foswiki::Func
package Foswiki::Plugins::ReadWriteOfflinePlugin::Store::Func;

use strict;
use Assert;
use IO::Handle;

sub new {
    my ($class, $session, $web, $topic, $attachment) = @_;
    if (ref($web)) {
        ($web, $topic, $attachment) = @$web;
    }
    my $this = bless({
        session => $session,
        web => $web,
        topic => $topic,
        attachment => $attachment,
    }, $class);
    return $this;
}

# Get given rev
sub getRevision {
    my ( $this, $version ) = @_;
    if ($this->{attachment}) {
        return Foswiki::Func::readAttachment(
            $this->{web}, $this->{topic}, $this->{attachment}, $version);
    } elsif ($this->{topic}) {
        return Foswiki::Func::readTopicText(
            $this->{web}, $this->{topic}, $version);
    } else {
        die "getRevision on web not supported";
    }
}

sub _infoCache {
    my ($this) = @_;
    if (!$this->{revInfoCache}) {
        my ( $date, $user, $rev, $comment ) =
          Foswiki::Func::getRevisionInfo(
              $this->{web}, $this->{topic}, undef, $this->{attachment});
        $this->{revInfoCache} = {
            date => $date,
            user => $user,
            rev => $rev,
            comment => $comment,
        };
    }
    return $this->{revInfoCache};
}

# Get number of current rev
sub currentRevision {
    my ($this) = @_;
    return $this->_infoCache()->{rev};
}

# The text has meta embedded, so if the store needs to store those separately
# it needs to separate them.
sub addRevision {
    my ($this, $text) = @_;
    if ($this->{attachment}) {
        my $tf = new File::Temp( UNLINK => 1 );
        print $tf $text;
        $tf->flush();
        $tf->seek(0, SEEK_SET);

        Foswiki::Func::saveAttachment(
            $this->{web}, $this->{topic}, $this->{attachment},
            {
                dontlog => 1,
                comment => "synched",
                stream => $tf,
            });
        close($tf);
    } elsif ($this->{topic}) {
        Foswiki::Func::saveTopicText(
            $this->{web}, $this->{topic},
              $text, 'Added by ReadWriteOfflinePlugin', 1, 1);
    } else {
        die "addRevision on web not supported";
    }
    return $this->currentRevision();
}

# SMELL: the only way to delete a web or topic is to move it to an existing
# web. Because we want to avoiod cluttering up the Trash web, we define
# a special web which cannot be synched.
# This is preloaded into $Foswiki::Plugins::ReadWriteOfflinePlugin::trashWeb
sub remove {
    my $this = shift;
    my $toWeb = $Foswiki::Plugins::ReadWriteOfflinePlugin::trashWeb;

    # The D\d{6}T\d{6} makes the date easy to recognise and extract
    my $when = Foswiki::Time::formatTime(
        time(), 'D$year$mo$dayT$hour$min$sec');

    # Convert the source web path to a topic name
    my $targetWeb = $this->{web};
    $targetWeb =~ s#[./]#_#g;

    if (!$this->{topic}) {
        # Append the event time to the trashed web name
        $targetWeb = "$toWeb/$when$targetWeb";
        my $i = '';
        while (Foswiki::Func::webExists("$targetWeb$i")) {
            $i++;
        }
        Foswiki::Func::moveWeb( $this->{web}, "$targetWeb$i");
        return;
    }

    # Target topic for web and topic deletion. Encodes the source web,
    # the topic name, and the time of the event.
    my $toTopic = "${targetWeb}_${when}_$this->{topic}";

    if (!$this->{attachment}) {
        # deleting a topic
        my $i = '';
        while (Foswiki::Func::topicExists($toWeb, "$toTopic$i")) {
            $i++;
        }
        moveTopic( $this->{web}, $this->{topic}, $toWeb, "$toTopic$i" );
        return;
    }

    # Deleting an attachment
    if (!Foswiki::Func::topicExists($toWeb, $toTopic)) {
        # Create the target topic if necessary
        Foswiki::Func::saveTopic(toWeb, $toTopic, undef, '');
    }

    my $toAttachment = $this->{attachment};
    my $i = '';
    while (Foswiki::Func::attachmentExists(
        $toWeb, $toTopic, "$toAttachment$i")) {
        # Unlikely, given that the topic is time-specific
        $i++;
    }
    Foswiki::Func::moveAttachment(
        $this->{web}, $this->{topic}, $this->{attachment},
        $toWeb, $toTopic, "$toAttachment$i" )
}

# -> ($rev, $date, $user, $comment)
sub getInfo {
    my $this = shift;
    my $cache = $this->_infoCache();

    return ($cache->{rev}, $cache->{date}, $cache->{user}, $cache->{comment});
}

# Get an iterator over the topics in a web
sub eachTopic {
    my $this = shift;
    my @list = Foswiki::Func::getTopicList($this->{web});
    return new Foswiki::ListIterator(\@list);
}

# Get an iterator over the attachments on a topic
sub eachAttachment {
    my $this = shift;
    my @list = Foswiki::Func::getAttachmentList($this->{web}, $this->{topic});
    return new Foswiki::ListIterator(\@list);
}

# Get an iterator over subweb names
sub eachSubWeb {
    my $this = shift;
    my @list = Foswiki::Func::getListOfWebs( undef, $this->{web} );
    return new Foswiki::ListIterator(\@list);
}

1;
