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

package Foswiki::Plugins::ReadWriteOfflinePlugin::Store::RCS;

use strict;
use Assert;

# facade for store operations, helps isolate the RcsFile impl from
# the rest of the code

sub new {
    my $class = shift;
    my $session = shift;
    my $handler;
    if (ref($_[0])) {
        $handler = $session->{store}->_getHandler(@{$_[0]});
    } else {
        $handler = $session->{store}->_getHandler(@_);
    }
    my $this = bless({
        session => $session,
        handler => $handler,
    }, $class);
    return $this;
}

# Get given rev
sub getRevision {
    my ( $this, $version ) = @_;
    return $this->{handler}->getRevision(@_);
}

# Get number of current rev
sub currentRevision {
    my ($this) = @_;
    return $this->{handler}->numRevisions(@_);
}

# The text has meta embedded, so if the store needs to store those separately
# it needs to separate them.
sub addRevision {
    my ($this, $text) = @_;
    $this->{handler}->addRevisionFromText(
        $text, 'Added by ReadWriteOfflinePlugin', Foswiki::Func::getWikiName());
    return $this->currentRevision();
}

# SMELL: this is a bit of a mess, because the RcsFile API doesn't
# support deletion of versioned objects (though it's fine with webs!)
sub remove {
    my $this = shift;
    my $handler = $this->{handler};

    if (!$handler->{topic}) {
        $handler->removeWeb();
        return;
    }
    unlink($handler->{file});
    unlink($handler->{rcsFile});
    if (!$handler->{attachment}) {
        # deleting a topic
        my $pub = $Foswiki::cfg{PubDir}.'/'.$handler->{web}.'/'.
          $handler->{topic};
        if (-d $pub) {
            Foswiki::Store::RcsFile::_rmtree($pub);
        }
    }
}

# -> ($rev, $date, $user, $comment)
sub getInfo {
    my $this = shift;
    my ($rev, $date, $user, $comment) = $this->{handler}->getRevisionInfo();
    # Stretch the date if required by the file timestamp. This is a workaround
    # for a core bug.
    my $modtime = (stat $this->{handler}->{file})[9];
    $date = $modtime if $modtime > $date;
    return ($rev, $date, $user, $comment);
}

{
    # Someday replace this with Foswiki::ListIterator
    package PrivateListIterator;

    sub new {
        my ($class, $list) = @_;
        my $this = bless({
            list => $list,
            index => 0,
            next => undef,
        }, $class);
        return $this;
    }

    sub hasNext {
        my( $this ) = @_;
        return 1 if $this->{next};
        my $n;
        if( $this->{list} && $this->{index} < scalar(@{$this->{list}}) ) {
            $n = $this->{list}->[$this->{index}++];
        } else {
            return 0;
        }
        $this->{next} = $n;
        return 1;
    }

    sub next {
        my $this = shift;
        $this->hasNext();
        my $n = $this->{next};
        $this->{next} = undef;
        return $n;
    }
}

# Get an iterator over the topics in a web
sub eachTopic {
    my $this = shift;
    ASSERT(!$this->{handler}->{topic}); # not sensible

    my @list = $this->{handler}->getTopicNames();
    return new PrivateListIterator(\@list);
}

# Get an iterator over the attachments on a topic
sub eachAttachment {
    my $this = shift;
    ASSERT($this->{handler}->{topic});
    ASSERT(!$this->{handler}->{attachment}); # not sensible

    my %atts = $this->{handler}->getAttachmentList(
        $this->{handler}->{web}, $this->{handler}->{topic});
    my @list = keys %atts;
    return new PrivateListIterator(\@list);
}

# Get an iterator over subweb names
sub eachSubWeb {
    my $this = shift;
    ASSERT(!$this->{handler}->{topic});

    my @list = $this->{handler}->getWebNames();
    return new PrivateListIterator(\@list);
}

1;
