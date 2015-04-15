#---+ Extensions
#---++ ReadWriteOfflinePlugin
# **STRING 30**
# Local ID, used in conflict markers. Should be unique for each client.
$Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{LocalID} = 'ExampleClient';
# **SELECTCLASS Foswiki::Plugins::ReadWriteOfflinePlugin::Store::* EXPERT**
# Select the compatible store implementation. The default, Func, is
# the most generic.
$Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{Store} = 'Foswiki::Plugins::ReadWriteOfflinePlugin::Store::Func';
# **PERL**
# Server specifications. Each server has to have <code>DefaultUrlHost</code>,
# <code>ScriptUrlPath</code> and <code>ScriptSuffix</code> set to the
# corresponding values from <code>configure</code> on that server.
# <code>Username</code> and <code>Password</code> must contain the
# login credentials for a user on the server. Both TemplateLogin and
# ApacheLogin type logins are supported.<p />
# 'loopback' is used for testing, in which case it must have a valid
# username and password for this server.
$Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{Servers} = {
    ExampleServer =>  {
        DefaultUrlHost => 'http://example.com',
        ScriptUrlPath => '/twiki/bin/',
        ScriptSuffix => '.pl',
        Username => 'LoginName',
        Password => 'secret',
    },
    # Loopback to this server, used for testing
    loopback => {
    },
};
# **STRING 30 EXPERT**
# Name of trash web on this site. The trash web cannot be synched - it is
# used exclusively for topic/web/attachment deletion. Note that you can't
# synch a local web with the trash web on a remote site either. By default
# this is the standard Trash web.
$Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{Trash} = '$Foswiki::cfg{TrashWebName}';

1;
