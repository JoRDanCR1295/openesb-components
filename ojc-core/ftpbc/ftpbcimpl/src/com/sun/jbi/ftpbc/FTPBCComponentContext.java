/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)FTPBCComponentContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;

import java.util.Arrays;
import java.util.List;

/**
 * singleton used to share the component context within the bc
 */
public class FTPBCComponentContext {

    public static final List TOKEN_DB_REGISTRY_TABLE_INFO = Arrays.asList(new String[]{"TOKEN_REGISTRY", "TOKEN_ID", "TOKEN_TABLE", "T_STAMP", "REF_COUNT"});
    public static final List TOKEN_DB_TOKEN_TABLE_INFO = Arrays.asList(new String[]{"TOKEN_ID", "VARCHAR(76)", "T_STAMP", "TIMESTAMP"});
    // const for installer mbeans and runtime configuration
    public static final String CONFIG_OUTBOUND_THREADS = "OutboundThreads";
    public static final String CONFIG_INVOKE_TIMEOUT = "InvokeTimeout";
    public static final String CONFIG_USE_PROXY = "UseProxy";
    public static final String CONFIG_PROXY_URL = "ProxyURL";
    public static final String CONFIG_PROXY_USR_ID = "ProxyUserID";
    public static final String CONFIG_PROXY_USR_PASSWD = "ProxyUserPassword";
    public static final String CONFIG_USE_PASSIVE_FTP = "UsePassiveFTP";
    public static final String CONFIG_POOL_MIN_SZ = "ConnectionPoolMinSize";
    public static final String CONFIG_POOL_MAX_SZ = "ConnectionPoolMaxSize";
    public static final String CONFIG_CONN_MAX_IDLE_TIMEOUT = "ConnectionMaxIdleTimeout";
    public static final String CONFIG_ENABLE_NM_PROPS = "EnableNMProps";
//    public static final String CONFIG_ENABLE_CLUSTER_AWARE = "EnableClusterAware";
//    public static final String CONFIG_TOKEN_PERSIST_URL = "TokenPersistenceURL";
//    public static final String CONFIG_DB_DRV_CLASS = "TokenDBJDBCDriverClass";
    // Configuration validation settings    
    public static int MIN_OUTBOUND_THREADS = 5;
    public static int MAX_OUTBOUND_THREADS = 2147483647;
    public static final String PROP_JBI_IS_CLUSTERED = "com.sun.jbi.isClustered";
    public static final String PROP_IS_CLUSTERED = "com.sun.jbi.ftpbc.isClustered";
    public static final String PROP_TOKEN_PERSISTENCE_URL = "com.sun.jbi.ftpbc.token.persistence.url";
    public static final String PROP_TOKEN_DB_DRV_CLAZZ = "com.sun.jbi.ftpbc.token.db.jdbc.driver";
    public static final String PROP_FAULTCODE = "com.sun.jbi.crl.faultcode";
    public static final String PROP_FAULTSTRING = "com.sun.jbi.crl.faultstring";
    public static final String PROP_FAULTACTOR = "com.sun.jbi.crl.faultactor";
    public static final String PROP_FAULTDETAIL = "com.sun.jbi.crl.faultdetail";
    public static final String FAULTCODE_SERVER = "Server";
    public static final String FAULTCODE_CLIENT = "Client";
    // NM properties
    public static final String NM_PROP_GROUP_ID = "org.glassfish.openesb.messaging.groupid";
    public static final String NM_PROP_MESSAGE_ID = "org.glassfish.openesb.messaging.messageid";
    public static final String NM_PROP_MESSAGE_LASTREC = "org.glassfish.openesb.messaging.lastrecord";
    public static final String NM_PROP_EP_NAME = "org.glassfish.openesb.exchange.endpointname";
    // ftp bc specific NM properties:
    public static final String NM_PROP_FTPBC_USE_DYN_EP_BINDING = "org.glassfish.openesb.ftp.use.dynamic.endpoint";
    // ftp:address
    public static final String NM_PROP_FTPBC_HOST = "org.glassfish.openesb.ftp.host";
    public static final String NM_PROP_FTPBC_PORT = "org.glassfish.openesb.ftp.port";
    public static final String NM_PROP_FTPBC_USERID = "org.glassfish.openesb.ftp.user";
    public static final String NM_PROP_FTPBC_PASSWORD = "org.glassfish.openesb.ftp.password";
    public static final String NM_PROP_FTPBC_DIR_LIST_TYLE = "org.glassfish.openesb.ftp.dir.list.style";
    public static final String NM_PROP_FTPBC_USE_UD_LIST_STYLE = "org.glassfish.openesb.ftp.use.user.dir.list.style";
    public static final String NM_PROP_FTPBC_UD_STYLE_NAME = "org.glassfish.openesb.ftp.user.dir.style.name";
    public static final String NM_PROP_FTPBC_UD_STYLE_CFG_LOC = "org.glassfish.openesb.ftp.user.dir.style.config";
    public static final String NM_PROP_FTPBC_MODE = "org.glassfish.openesb.ftp.mode";
    public static final String NM_PROP_FTPBC_CNTRL_ENCODING = "org.glassfish.openesb.ftp.control.channel.encoding";
    public static final String NM_PROP_FTPBC_CNTRL_CH_TIMEOUT = "org.glassfish.openesb.ftp.control.channel.timeout";
    public static final String NM_PROP_FTPBC_DATA_CH_TIMEOUT = "org.glassfish.openesb.ftp.data.channel.timeout";
    public static final String NM_PROP_FTPBC_SECURE_FTP_TYPE = "org.glassfish.openesb.ftp.secure.type";
    public static final String NM_PROP_FTPBC_ENABLE_CCC = "org.glassfish.openesb.ftp.enable.ccc";
    public static final String NM_PROP_FTPBC_KEYSTORE_LOC = "org.glassfish.openesb.ftp.keystore.location";
    public static final String NM_PROP_FTPBC_KEYSTORE_PASSWORD = "org.glassfish.openesb.ftp.keystore.password";
    public static final String NM_PROP_FTPBC_TRUSTSTORE_LOC = "org.glassfish.openesb.ftp.truststore.location";
    public static final String NM_PROP_FTPBC_TRUSTSTORE_PASSWORD = "org.glassfish.openesb.ftp.truststore.password";
    public static final String NM_PROP_FTPBC_KEYALIAS = "org.glassfish.openesb.ftp.key.alias";
    public static final String NM_PROP_FTPBC_KEYPASSWORD = "org.glassfish.openesb.ftp.key.alias.password";
    // ftp:message
    public static final String NM_PROP_FTPBC_MSG_REPO = "org.glassfish.openesb.ftp.msg.repository";
    public static final String NM_PROP_FTPBC_MSG_NAME = "org.glassfish.openesb.ftp.msg.name";
    public static final String NM_PROP_FTPBC_MSG_NAME_PREFIX = "org.glassfish.openesb.ftp.msg.prefix";
    public static final String NM_PROP_FTPBC_MSG_REQ_RESP_CORRELATE = "org.glassfish.openesb.ftp.msg.req.resp.correlate";
    public static final String NM_PROP_FTPBC_MSG_PUT_DIR = "org.glassfish.openesb.ftp.msg.put.dir";
    public static final String NM_PROP_FTPBC_MSG_PUT_FILE = "org.glassfish.openesb.ftp.msg.put.file";
    public static final String NM_PROP_FTPBC_MSG_GET_DIR = "org.glassfish.openesb.ftp.msg.get.dir";
    public static final String NM_PROP_FTPBC_MSG_GET_FILE = "org.glassfish.openesb.ftp.msg.get.file";
    public static final String NM_PROP_FTPBC_MSG_PRE_PUT_DIR = "org.glassfish.openesb.ftp.msg.pre.put.dir";
    public static final String NM_PROP_FTPBC_MSG_PRE_PUT_FILE = "org.glassfish.openesb.ftp.msg.pre.put.file";
    public static final String NM_PROP_FTPBC_MSG_POST_PUT_DIR = "org.glassfish.openesb.ftp.msg.post.put.dir";
    public static final String NM_PROP_FTPBC_MSG_POST_PUT_FILE = "org.glassfish.openesb.ftp.msg.post.put.file";
    public static final String NM_PROP_FTPBC_MSG_PRE_GET_DIR = "org.glassfish.openesb.ftp.msg.pre.get.dir";
    public static final String NM_PROP_FTPBC_MSG_PRE_GET_FILE = "org.glassfish.openesb.ftp.msg.pre.get.file";
    public static final String NM_PROP_FTPBC_MSG_POST_GET_DIR = "org.glassfish.openesb.ftp.msg.post.get.dir";
    public static final String NM_PROP_FTPBC_MSG_POST_GET_FILE = "org.glassfish.openesb.ftp.msg.post.get.file";
    // ftp:transfer
    public static final String NM_PROP_FTPBC_TRANS_SEND_TO = "org.glassfish.openesb.ftp.transfer.destination";
    public static final String NM_PROP_FTPBC_TRANS_APPEND = "org.glassfish.openesb.ftp.transfer.append";
    public static final String NM_PROP_FTPBC_TRANS_RECV_FROM = "org.glassfish.openesb.ftp.transfer.source";
    public static final String NM_PROP_FTPBC_TRANS_DEST_PATT = "org.glassfish.openesb.ftp.transfer.dest.is.pattern";
    public static final String NM_PROP_FTPBC_TRANS_SRC_REGEX = "org.glassfish.openesb.ftp.transfer.src.is.regex";
    public static final String NM_PROP_FTPBC_TRANS_PRE_SEND_CMD = "org.glassfish.openesb.ftp.transfer.pre.send.cmd";
    public static final String NM_PROP_FTPBC_TRANS_POST_SEND_CMD = "org.glassfish.openesb.ftp.transfer.post.send.cmd";
    public static final String NM_PROP_FTPBC_TRANS_PRE_SEND_LOC = "org.glassfish.openesb.ftp.transfer.pre.send.location";
    public static final String NM_PROP_FTPBC_TRANS_PRE_SEND_LOC_HAS_PATT = "org.glassfish.openesb.ftp.transfer.pre.send.location.has.pattern";
    public static final String NM_PROP_FTPBC_TRANS_POST_SEND_LOC = "org.glassfish.openesb.ftp.transfer.post.send.location";
    public static final String NM_PROP_FTPBC_TRANS_POST_SEND_LOC_HAS_PATT = "org.glassfish.openesb.ftp.transfer.post.send.location.has.pattern";
    public static final String NM_PROP_FTPBC_TRANS_PRE_RECV_CMD = "org.glassfish.openesb.ftp.transfer.pre.recv.cmd";
    public static final String NM_PROP_FTPBC_TRANS_POST_RECV_CMD = "org.glassfish.openesb.ftp.transfer.post.recv.cmd";
    public static final String NM_PROP_FTPBC_TRANS_PRE_RECV_LOC = "org.glassfish.openesb.ftp.transfer.pre.recv.location";
    public static final String NM_PROP_FTPBC_TRANS_PRE_RECV_LOC_HAS_PATT = "org.glassfish.openesb.ftp.transfer.pre.recv.location.has.pattern";
    public static final String NM_PROP_FTPBC_TRANS_POST_RECV_LOC = "org.glassfish.openesb.ftp.transfer.post.recv.location";
    public static final String NM_PROP_FTPBC_TRANS_POST_RECV_LOC_HAS_PATT = "org.glassfish.openesb.ftp.transfer.post.recv.location.has.pattern";
    public static final String NM_PROP_FTPBC_TRANS_REQ_RESP_CORRELATE = "org.glassfish.openesb.ftp.transfer.req.resp.correlate";
    public static final String NM_PROP_FTPBC_TRANS_PUT_DIR = "org.glassfish.openesb.ftp.transfer.put.dir";
    public static final String NM_PROP_FTPBC_TRANS_PUT_FILE = "org.glassfish.openesb.ftp.transfer.put.file";
    public static final String NM_PROP_FTPBC_TRANS_GET_DIR = "org.glassfish.openesb.ftp.transfer.get.dir";
    public static final String NM_PROP_FTPBC_TRANS_GET_FILE = "org.glassfish.openesb.ftp.transfer.get.file";
    public static final String NM_PROP_FTPBC_TRANS_PRE_PUT_DIR = "org.glassfish.openesb.ftp.transfer.pre.send.dir";
    public static final String NM_PROP_FTPBC_TRANS_PRE_PUT_FILE = "org.glassfish.openesb.ftp.transfer.pre.send.file";
    public static final String NM_PROP_FTPBC_TRANS_POST_PUT_DIR = "org.glassfish.openesb.ftp.transfer.post.send.dir";
    public static final String NM_PROP_FTPBC_TRANS_POST_PUT_FILE = "org.glassfish.openesb.ftp.transfer.post.send.file";
    public static final String NM_PROP_FTPBC_TRANS_PRE_GET_DIR = "org.glassfish.openesb.ftp.transfer.pre.recv.dir";
    public static final String NM_PROP_FTPBC_TRANS_PRE_GET_FILE = "org.glassfish.openesb.ftp.transfer.pre.recv.file";
    public static final String NM_PROP_FTPBC_TRANS_POST_GET_DIR = "org.glassfish.openesb.ftp.transfer.post.recv.dir";
    public static final String NM_PROP_FTPBC_TRANS_POST_GET_FILE = "org.glassfish.openesb.ftp.transfer.post.recv.file";
    private static FTPBCComponentContext instance = new FTPBCComponentContext();
    private ComponentContext context;
    private MessagingChannel channel;
    private ComponentLifeCycle lifeCycle;

    private FTPBCComponentContext() {
    }

    public static FTPBCComponentContext getInstance() {
        return instance;
    }

    public ComponentContext getContext() {
        return context;
    }

    public void setContext(ComponentContext context) {
        this.context = context;
    }

    /**
     * @return the component lifecycle associated with this context
     * if it has been initialized
     */
    public ComponentLifeCycle getAssociatedLifeCycle() {
        return lifeCycle;
    }

    /**
     * Set the component lifecycle associated with this context
     */
    public void setAssociatedLifeCycle(ComponentLifeCycle aLifeCycle) {
        lifeCycle = aLifeCycle;
    }

    /**
     * @return Obtain the channel associated with this context
     * if it has been initialized
     */
    public MessagingChannel getBindingChannel() {
        return channel;
    }

    /**
     * Set the initizalied channel associated with this context
     */
    public void setBindingChannel(MessagingChannel aChannel) {
        channel = aChannel;
    }
}
