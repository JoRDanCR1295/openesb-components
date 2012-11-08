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
 * @(#)JavaMailProperties.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.binding.email.protocol;

import java.util.Properties;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 */
public class JavaMailProperties {
    private static Properties mMailProperties;
    private static Properties mInternetProperties;
    private static Properties mImapProperties;
    private static Properties mPop3Properties;
    private static Properties mSmtpProperties;

    /**
     * @param sessionProps
     * @return
     */
    public static Properties getMailProperties(Properties sessionProps) {
        if (null == mMailProperties) {
            mMailProperties = loadMailProperties(sessionProps);
        }
        return mMailProperties;
    }
    
    /**
     * @return
     */
    public static Properties getInternetProperties() { // from System.getProperties()
        if (null == mInternetProperties) {
            mInternetProperties = loadInternetProperties();
        }
        return mInternetProperties;
    }
    
    /**
     * @param sessionProps
     * @return
     */
    public static Properties getImapProperties(Properties sessionProps) {
        if (null == mImapProperties) {
            mImapProperties = loadImapProperties(sessionProps);
        }
        return mImapProperties;
    }
    
    /**
     * @param sessionProps
     * @return
     */
    public static Properties getPop3Properties(Properties sessionProps) {
        if (null == mPop3Properties) {
            mPop3Properties = loadPop3Properties(sessionProps);
        }
        return mPop3Properties;
    }
    
    /**
     * @param sessionProps
     * @return
     */
    public static Properties getSmtpProperties(Properties sessionProps) {
        if (null == mSmtpProperties) {
            mSmtpProperties = loadSmtpProperties(sessionProps);
        }
        return mSmtpProperties;
    }
    
    /**
     * @param sessionProps
     * @return
     */
    public static Properties loadMailProperties(Properties sessionProps) {
        mMailProperties = new Properties();
        mMailProperties.put(EmailBCConstants.SESSION_MAIL_DEBUG, sessionProps.get(EmailBCConstants.SESSION_MAIL_DEBUG) + "");
        mMailProperties.put(EmailBCConstants.SESSION_MAIL_FROM, sessionProps.get(EmailBCConstants.SESSION_MAIL_FROM) + "");
        mMailProperties.put(EmailBCConstants.SESSION_MAIL_MIME_ADDRESS_STRICT, sessionProps.get(EmailBCConstants.SESSION_MAIL_MIME_ADDRESS_STRICT) + "");
        mMailProperties.put(EmailBCConstants.SESSION_MAIL_HOST, sessionProps.get(EmailBCConstants.SESSION_MAIL_HOST) + "");
        mMailProperties.put(EmailBCConstants.SESSION_MAIL_STORE_PROTOCOL, sessionProps.get(EmailBCConstants.SESSION_MAIL_STORE_PROTOCOL) + "");
        mMailProperties.put(EmailBCConstants.SESSION_MAIL_TRANSPORT_PROTOCOL, sessionProps.get(EmailBCConstants.SESSION_MAIL_TRANSPORT_PROTOCOL) + "");
        mMailProperties.put(EmailBCConstants.SESSION_MAIL_USER, sessionProps.get(EmailBCConstants.SESSION_MAIL_USER) + "");
        //mMailProperties.put(EmailBCConstants.SESSION_MAIL_PROTOCOL_CLASS, sessionProps.get(EmailBCConstants.SESSION_MAIL_PROTOCOL_CLASS) + "");
        //mMailProperties.put(EmailBCConstants.SESSION_MAIL_PROTOCOL_HOST, sessionProps.get(EmailBCConstants.SESSION_MAIL_PROTOCOL_HOST) + "");
        //mMailProperties.put(EmailBCConstants.SESSION_MAIL_PROTOCOL_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_PROTOCOL_PORT) + "");
        //mMailProperties.put(EmailBCConstants.SESSION_MAIL_PROTOCOL_USER, sessionProps.get(EmailBCConstants.SESSION_MAIL_PROTOCOL_USER) + "");
        
        return mMailProperties;
    }
  
    /**
     * @return
     */
    public static Properties loadInternetProperties() { // from System.getProperties()
        mInternetProperties = new Properties();
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_ADDRESS_STRICT, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_ADDRESS_STRICT) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_CHARSET, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_CHARSET) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_DECODETEXT_STRICT, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_DECODETEXT_STRICT) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_ENCODEEOL_STRICT, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_ENCODEEOL_STRICT) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_DECODEFILENAME, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_DECODEFILENAME) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_ENCODEFILENAME, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_ENCODEFILENAME) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_DECODEPARAMETERS, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_DECODEPARAMETERS) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_ENCODEPARAMETERS, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_ENCODEPARAMETERS) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_MULTIPART_IGNOREMISSINGENDBOUNDARY, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_MULTIPART_IGNOREMISSINGENDBOUNDARY) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_MULTIPART_IGNOREMISSINGBOUNDARYPARAMETER, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_MULTIPART_IGNOREMISSINGBOUNDARYPARAMETER) + "");
        //SUN specific
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_BASE64_IGNOREERRORS, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_BASE64_IGNOREERRORS) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_FOLDTEXT, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_FOLDTEXT) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_SETCONTENTTYPEFILENAME, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_SETCONTENTTYPEFILENAME) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_SETDEFAULTTEXTCHARSET, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_SETDEFAULTTEXTCHARSET) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_PARAMETERS_STRICT, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_PARAMETERS_STRICT) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_APPLEFILENAMES, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_APPLEFILENAMES) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_IGNOREUNKNOWNENCODING, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_IGNOREUNKNOWNENCODING) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_UUDECODE_IGNOREERRORS, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_UUDECODE_IGNOREERRORS) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_UUDECODE_IGNOREMISSINGBEGINEND, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_UUDECODE_IGNOREMISSINGBEGINEND) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_MULTIPART_ALLOWEMPTY, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_MULTIPART_ALLOWEMPTY) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_MIME_MULTIPART_IGNOREEXISTINGBOUNDARYPARAMETER, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_MIME_MULTIPART_IGNOREEXISTINGBOUNDARYPARAMETER) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_ALTERNATES, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_ALTERNATES) + "");
        mInternetProperties.put(EmailBCConstants.SYSTEM_MAIL_REPLYALLCC, System.getProperties().get(EmailBCConstants.SYSTEM_MAIL_REPLYALLCC) + "");

        return mInternetProperties;
    }
    
    /**
     * @param sessionProps
     * @return
     */
    public static Properties loadImapProperties(Properties sessionProps) {
        mImapProperties = new Properties();
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_USER, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_USER) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_HOST, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_HOST) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_PORT) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_PARTIALFETCH, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_PARTIALFETCH) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_FETCHSIZE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_FETCHSIZE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_CONNECTIONTIMEOUT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_CONNECTIONTIMEOUT) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_TIMEOUT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_TIMEOUT) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_STATUSCACHETIMEOUT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_STATUSCACHETIMEOUT) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_APPENDBUFFERSIZE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_APPENDBUFFERSIZE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_CONNECTIONPOOLSIZE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_CONNECTIONPOOLSIZE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_CONNECTIONPOOLTIMEOUT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_CONNECTIONPOOLTIMEOUT) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SEPARATESTORECONNECTION, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SEPARATESTORECONNECTION) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_ALLOWREADONLYSELECT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_ALLOWREADONLYSELECT) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_AUTH_LOGIN_DISABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_AUTH_LOGIN_DISABLE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_AUTH_PLAIN_DISABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_AUTH_PLAIN_DISABLE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_PROXYAUTH_USER, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_PROXYAUTH_USER) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_STARTTLS_ENABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_STARTTLS_ENABLE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_STARTTLS_REQUIRED, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_STARTTLS_REQUIRED) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_LOCALADDRESS, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_LOCALADDRESS) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_LOCALPORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_LOCALPORT) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SASL_ENABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SASL_ENABLE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SASL_MECHANISMS, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SASL_MECHANISMS) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SASL_AUTHORIZATIONID, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SASL_AUTHORIZATIONID) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SASL_REALM, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SASL_REALM) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SASL_XGWTRUSTEDAPPHACK_ENABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SASL_XGWTRUSTEDAPPHACK_ENABLE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SOCKETFACTORY, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SOCKETFACTORY) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SOCKETFACTORY_CLASS, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SOCKETFACTORY_CLASS) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SOCKETFACTORY_FALLBACK, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SOCKETFACTORY_FALLBACK) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SOCKETFACTORY_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SOCKETFACTORY_PORT) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SSL_ENABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SSL_ENABLE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SSL_CHECKSERVERIDENTITY, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SSL_CHECKSERVERIDENTITY) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SSL_SOCKETFACTORY, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SSL_SOCKETFACTORY) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SSL_SOCKETFACTORY_CLASS, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SSL_SOCKETFACTORY_CLASS) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SSL_SOCKETFACTORY_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SSL_SOCKETFACTORY_PORT) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SSL_PROTOCOLS, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SSL_PROTOCOLS) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_SSL_CIPHERSUITES, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SSL_CIPHERSUITES) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_MINIDLETIME, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_MINIDLETIME) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAP_ENABLEIMAPEVENTS, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_ENABLEIMAPEVENTS) + "");
        
        //imaps
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_USER, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_USER) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_HOST, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_HOST) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_PORT) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_PARTIALFETCH, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_PARTIALFETCH) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_FETCHSIZE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_FETCHSIZE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_CONNECTIONTIMEOUT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_CONNECTIONTIMEOUT) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_TIMEOUT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_TIMEOUT) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_STATUSCACHETIMEOUT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_STATUSCACHETIMEOUT) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_APPENDBUFFERSIZE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_APPENDBUFFERSIZE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_CONNECTIONPOOLSIZE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_CONNECTIONPOOLSIZE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_CONNECTIONPOOLTIMEOUT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_CONNECTIONPOOLTIMEOUT) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SEPARATESTORECONNECTION, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SEPARATESTORECONNECTION) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_ALLOWREADONLYSELECT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_ALLOWREADONLYSELECT) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_AUTH_LOGIN_DISABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_AUTH_LOGIN_DISABLE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_AUTH_PLAIN_DISABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_AUTH_PLAIN_DISABLE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_PROXYAUTH_USER, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_PROXYAUTH_USER) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_STARTTLS_ENABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_STARTTLS_ENABLE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_STARTTLS_REQUIRED, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_STARTTLS_REQUIRED) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_LOCALADDRESS, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_LOCALADDRESS) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_LOCALPORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_LOCALPORT) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SASL_ENABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SASL_ENABLE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SASL_MECHANISMS, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SASL_MECHANISMS) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SASL_AUTHORIZATIONID, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SASL_AUTHORIZATIONID) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SASL_REALM, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SASL_REALM) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SASL_XGWTRUSTEDAPPHACK_ENABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SASL_XGWTRUSTEDAPPHACK_ENABLE) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SOCKETFACTORY, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SOCKETFACTORY) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SOCKETFACTORY_CLASS, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SOCKETFACTORY_CLASS) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SOCKETFACTORY_FALLBACK, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SOCKETFACTORY_FALLBACK) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SOCKETFACTORY_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SOCKETFACTORY_PORT) + "");
        //mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SSL_ENABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SSL_ENABLE) + "");
        //mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SSL_CHECKSERVERIDENTITY, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SSL_CHECKSERVERIDENTITY) + "");
        //mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SSL_SOCKETFACTORY, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SSL_SOCKETFACTORY) + "");
        //mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SSL_SOCKETFACTORY_CLASS, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SSL_SOCKETFACTORY_CLASS) + "");
        //mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SSL_SOCKETFACTORY_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SSL_SOCKETFACTORY_PORT) + "");
        //mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SSL_PROTOCOLS, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SSL_PROTOCOLS) + "");
        //mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_SSL_CIPHERSUITES, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_SSL_CIPHERSUITES) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_MINIDLETIME, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_MINIDLETIME) + "");
        mImapProperties.put(EmailBCConstants.SESSION_MAIL_IMAPS_ENABLEIMAPEVENTS, sessionProps.get(EmailBCConstants.SESSION_MAIL_IMAP_ENABLEIMAPEVENTS) + "");
        
        return mImapProperties;
    }
    
    /**
     * @param sessionProps
     * @return
     */
    public static Properties loadPop3Properties(Properties sessionProps) {
        mPop3Properties = new Properties();
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_USER, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_USER) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_HOST, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_HOST) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_PORT) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_CONNECTIONTIMEOUT, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_CONNECTIONTIMEOUT) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_TIMEOUT, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_TIMEOUT) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_RSETBEFOREQUIT, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_RSETBEFOREQUIT) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_MESSAGE_CLASS, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_MESSAGE_CLASS) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_LOCALADDRESS, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_LOCALADDRESS) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_LOCALPORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_LOCALPORT) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_APOP_ENABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_APOP_ENABLE) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_SOCKETFACTORY, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SOCKETFACTORY) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_SOCKETFACTORY_CLASS, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SOCKETFACTORY_CLASS) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_SOCKETFACTORY_FALLBACK, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SOCKETFACTORY_FALLBACK) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_SOCKETFACTORY_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SOCKETFACTORY_PORT) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_SSL_ENABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SSL_ENABLE) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_SSL_CHECKSERVERIDENTITY, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SSL_CHECKSERVERIDENTITY) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_SSL_SOCKETFACTORY, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SSL_SOCKETFACTORY) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_SSL_SOCKETFACTORY_CLASS, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SSL_SOCKETFACTORY_CLASS) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_SSL_SOCKETFACTORY_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SSL_SOCKETFACTORY_PORT) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_SSL_PROTOCOLS, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SSL_PROTOCOLS) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_SSL_CIPHERSUITES, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SSL_CIPHERSUITES) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_DISABLETOP, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_DISABLETOP) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3_FORGETTOPHEADERS, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_FORGETTOPHEADERS) + "");
        
        //pop3s
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_USER, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_USER) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_HOST, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_HOST) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_PORT) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_CONNECTIONTIMEOUT, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_CONNECTIONTIMEOUT) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_TIMEOUT, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_TIMEOUT) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_RSETBEFOREQUIT, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_RSETBEFOREQUIT) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_MESSAGE_CLASS, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_MESSAGE_CLASS) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_LOCALADDRESS, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_LOCALADDRESS) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_LOCALPORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_LOCALPORT) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_APOP_ENABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_APOP_ENABLE) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_SOCKETFACTORY, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SOCKETFACTORY) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_SOCKETFACTORY_CLASS, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SOCKETFACTORY_CLASS) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_SOCKETFACTORY_FALLBACK, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SOCKETFACTORY_FALLBACK) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_SOCKETFACTORY_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SOCKETFACTORY_PORT) + "");
        //mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_SSL_ENABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SSL_ENABLE) + "");
        //mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_SSL_CHECKSERVERIDENTITY, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SSL_CHECKSERVERIDENTITY) + "");
        //mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_SSL_SOCKETFACTORY, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SSL_SOCKETFACTORY) + "");
        //mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_SSL_SOCKETFACTORY_CLASS, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SSL_SOCKETFACTORY_CLASS) + "");
        //mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_SSL_SOCKETFACTORY_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SSL_SOCKETFACTORY_PORT) + "");
        //mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_SSL_PROTOCOLS, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SSL_PROTOCOLS) + "");
        //mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_SSL_CIPHERSUITES, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_SSL_CIPHERSUITES) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_DISABLETOP, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_DISABLETOP) + "");
        mPop3Properties.put(EmailBCConstants.SESSION_MAIL_POP3S_FORGETTOPHEADERS, sessionProps.get(EmailBCConstants.SESSION_MAIL_POP3_FORGETTOPHEADERS) + "");
        
        return mPop3Properties;
    }
    
    /**
     * @param sessionProps
     * @return
     */
    public static Properties loadSmtpProperties(Properties sessionProps) {
        mSmtpProperties = new Properties();
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_USER, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_USER) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_HOST, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_HOST) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_PORT) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_CONNECTIONTIMEOUT, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_CONNECTIONTIMEOUT) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_TIMEOUT, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_TIMEOUT) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_FROM, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_FROM) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_LOCALHOST, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_LOCALHOST) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_LOCALADDRESS, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_LOCALADDRESS) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_LOCALPORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_LOCALPORT) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_EHLO, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_EHLO) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_AUTH, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_AUTH) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_AUTH_MECHANISMS, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_AUTH_MECHANISMS) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_SUBMITTER, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SUBMITTER) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_DSN_NOTIFY, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_DSN_NOTIFY) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_DSN_RET, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_DSN_RET) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_ALLOW8BITMIME, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_ALLOW8BITMIME) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_SENDPARTIAL, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SENDPARTIAL) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_SASL_REALM, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SASL_REALM) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_QUITWAIT, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_QUITWAIT) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_REPORTSUCCESS, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_REPORTSUCCESS) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_SOCKETFACTORY, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SOCKETFACTORY) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_SOCKETFACTORY_CLASS, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SOCKETFACTORY_CLASS) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_SOCKETFACTORY_FALLBACK, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SOCKETFACTORY_FALLBACK) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_SOCKETFACTORY_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SOCKETFACTORY_PORT) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_SSL_ENABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SSL_ENABLE) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_SSL_CHECKSERVERIDENTITY, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SSL_CHECKSERVERIDENTITY) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_SSL_SOCKETFACTORY, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SSL_SOCKETFACTORY) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_SSL_SOCKETFACTORY_CLASS, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SSL_SOCKETFACTORY_CLASS) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_SSL_SOCKETFACTORY_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SSL_SOCKETFACTORY_PORT) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_SSL_PROTOCOLS, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SSL_PROTOCOLS) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_SSL_CIPHERSUITES, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SSL_CIPHERSUITES) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_MAILEXTENSION, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_MAILEXTENSION) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_STARTTLS_ENABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_STARTTLS_ENABLE) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_STARTTLS_REQUIRED, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_STARTTLS_REQUIRED) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTP_USERSET, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_USERSET) + "");
        
        //smtps
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_USER, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_USER) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_HOST, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_HOST) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_PORT) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_CONNECTIONTIMEOUT, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_CONNECTIONTIMEOUT) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_TIMEOUT, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_TIMEOUT) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_FROM, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_FROM) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_LOCALHOST, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_LOCALHOST) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_LOCALADDRESS, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_LOCALADDRESS) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_LOCALPORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_LOCALPORT) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_EHLO, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_EHLO) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_AUTH, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_AUTH) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_AUTH_MECHANISMS, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_AUTH_MECHANISMS) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_SUBMITTER, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SUBMITTER) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_DSN_NOTIFY, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_DSN_NOTIFY) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_DSN_RET, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_DSN_RET) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_ALLOW8BITMIME, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_ALLOW8BITMIME) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_SENDPARTIAL, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SENDPARTIAL) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_SASL_REALM, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SASL_REALM) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_QUITWAIT, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_QUITWAIT) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_REPORTSUCCESS, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_REPORTSUCCESS) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_SOCKETFACTORY, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SOCKETFACTORY) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_SOCKETFACTORY_CLASS, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SOCKETFACTORY_CLASS) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_SOCKETFACTORY_FALLBACK, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SOCKETFACTORY_FALLBACK) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_SOCKETFACTORY_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SOCKETFACTORY_PORT) + "");
        //mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_SSL_ENABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SSL_ENABLE) + "");
        //mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_SSL_CHECKSERVERIDENTITY, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SSL_CHECKSERVERIDENTITY) + "");
        //mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_SSL_SOCKETFACTORY, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SSL_SOCKETFACTORY) + "");
        //mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_SSL_SOCKETFACTORY_CLASS, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SSL_SOCKETFACTORY_CLASS) + "");
        //mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_SSL_SOCKETFACTORY_PORT, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SSL_SOCKETFACTORY_PORT) + "");
        //mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_SSL_PROTOCOLS, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SSL_PROTOCOLS) + "");
        //mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_SSL_CIPHERSUITES, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_SSL_CIPHERSUITES) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_MAILEXTENSION, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_MAILEXTENSION) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_STARTTLS_ENABLE, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_STARTTLS_ENABLE) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_STARTTLS_REQUIRED, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_STARTTLS_REQUIRED) + "");
        mSmtpProperties.put(EmailBCConstants.SESSION_MAIL_SMTPS_USERSET, sessionProps.get(EmailBCConstants.SESSION_MAIL_SMTP_USERSET) + "");
        
        return mSmtpProperties;
    }
    
}
