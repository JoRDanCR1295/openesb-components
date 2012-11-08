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
 * @(#)EmailConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**************************************************************************
 *
 *          Copyright (c) 2003, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.smtpbc.extservice;

import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.mail.Authenticator;
import javax.mail.PasswordAuthentication;

import com.sun.jbi.internationalization.Messages;

/**
 * Class which holds email connection specific information.
 *
 * @author  
 * @version $Version$
 */
public class EmailConfiguration {
    private final Properties emailConfig;
    private static final Logger mLogger = Messages.getLogger(EmailConfiguration.class);
    /**
     * 
     */
    public static final String SSL_FACTORY = "javax.net.ssl.SSLSocketFactory";
    /**
     * 
     */
    public static final String MAIL_FACTORY_PORT = "mail.smtp.socketFactory.port";
    /**
     * 
     */
    public static final String MAIL_FACTORY_CLASS = "mail.smtp.socketFactory.class";
    /**
     * 
     */
    public static final String MAIL_FACTORY_FALLBACK = "mail.smtp.socketFactory.fallback";
    
    /**
     * Creates new EmailConfiguration.
     */
    public EmailConfiguration() {
        emailConfig = new Properties();
        _UserSend = "";
        _PasswordSend = "";
        _UserRecv = "";
        _PasswordRecv = "";
        _SessionAuth = false;
        _useSSL= false;
        
        try {
            initialize();
        } catch (final Exception ex) {
            mLogger.log(Level.SEVERE, ex.getMessage(), ex);
        }
    }


    /**
     * Initialize the EmailConfiguration instance with the Properties
     * bundle.
     *
     * @param   originalConfig  The Properties bundle containing the
     *                          configuration information.
     */
    public void initialize()
            throws EmailApplicationException {
        final String temp = null;
        emailConfig.clear();

        /**
         * Required by JavaMail in order to determine the message delivery
         * protocol for the session.
         */
        emailConfig.setProperty("mail.transport.protocol", "smtp");
        
        

        setHostSend("");
        setPortSend(25);
        setUserSend("");
        setUseSSL(false);

        try {
            setPasswordSend("");
        }
        catch (final Exception ex) {
            final String str =
                "EmailConfiguration.initialize(): " +
                "Wrong UserSend=" + getUserSend() + " or PasswordSend=" + temp + ",\n" +
                "Exception info: " + ex.toString();
            mLogger.severe(str);
            throw new EmailApplicationException (str);
        }

        /**
         * Required by JavaMail in order to determine the message retrieval
         * protocol for the session.
         */
        emailConfig.setProperty("mail.store.protocol", "pop3");

        setHostRecv("");
        setPortRecv(110);
        setUserRecv("");
        try {
            setPasswordRecv("");
        }
        catch (final Exception ex) {
            final String str =
                "EmailConfiguration.initialize(): " +
                "Wrong UserRecv=" + getUserRecv() + " or PasswordRecv=" + temp + ",\n" +
                "Exception info:" + ex.toString();
            mLogger.severe(str);
            throw new EmailApplicationException (str);
        }

        /**
         * Determine whether to perform POP3 authentication before attempting
         * SMTP transfer.
         */
        setSessionAuth(false);

        /**
         * Prepare for SSL
         */
//         aConfiguration.setSubSection("SSL");
//         try {
//             boolean recvSSL = aConfiguration.getStringParameterValue("Receive/UseSSL", "NO").equalsIgnoreCase("YES");
//             boolean sendSSL = aConfiguration.getStringParameterValue("Send/UseSSL", "NO").equalsIgnoreCase("YES");

//             // Set up the TrustStore
//             if (recvSSL || sendSSL) {

//                 if (mLogger.isDebugEnabled()) {
//                     System.setProperty("javax.net.debug", "all");
//                 }

//                 // Set up the trustore
//                 aConfiguration.setSubSection("SSL/CACerts");
//                 String trustStore = aConfiguration.getStringParameterValue("TrustStore", null);
//                 if (trustStore == null || trustStore.length() == 0) {
//                     String str =
//                         "EmailConfiguration.initialize(): " +
//                         "TrustStore is not specified in the configuration.";
//                     mLogger.error (str);
//                     throw new EmailApplicationException (str);
//                 }
//                 String trustStoreType = aConfiguration.getStringParameterValue("TrustStoreType", null);
//                 if (trustStoreType == null || trustStoreType.length() == 0) {
//                     String str =
//                         "EmailConfiguration.initialize(): " +
//                         "TrustStoreType is not specified in the configuration.";
//                     mLogger.error (str);
//                     throw new EmailApplicationException (str);
//                 }
//                 String trustStorePass = aConfiguration.getStringParameterValue("TrustStorePassword", null);
//                 if (trustStorePass == null || trustStorePass.length() == 0) {
//                     String str =
//                         "EmailConfiguration.initialize(): " +
//                         "TrustStorePassword is not specified in the configuration.";
//                     mLogger.error (str);
//                     throw new EmailApplicationException (str);
//                 }
//                 mLogger.info ("Using TrustStore " + trustStore + " of type " + trustStoreType);
//                 EmailSystemProperties.setTrustStore(trustStore);
//                 EmailSystemProperties.setTrustStoreType(trustStoreType);
//                 EmailSystemProperties.setTrustStorePassword(trustStorePass);

//                 // Setup receive
//                 if (recvSSL) {
//                     mLogger.info ("SSL is enabled for RECEIVE; using " + SSL_FACTORY + " for creating ssl sockets.");
//                     // POP3 provider
//                     this.emailConfig.setProperty( "mail.pop3.socketFactory.class", SSL_FACTORY);

//                     // POP3 provider - no fallback.
//                     this.emailConfig.setProperty( "mail.pop3.socketFactory.fallback", "false");
//                 } else {
//                     mLogger.debug ("SSL is NOT enabled for RECEIVE.");
//                 }

//                 // Setup send
//                 if (sendSSL) {
//                     mLogger.info ("SSL is enabled for SEND; using " + SSL_FACTORY + " for creating ssl sockets.");
//                     // SMTP provider
//                     this.emailConfig.setProperty( "mail.smtp.socketFactory.class", SSL_FACTORY);

//                     // SMTP provider - no fallback.
//                     this.emailConfig.setProperty( "mail.smtp.socketFactory.fallback", "false");
//                 } else {
//                     mLogger.debug ("SSL is NOT enabled for SEND.");
//                 }

//             } else {
//                 mLogger.debug ("SSL is NOT enabled.");
//             }

        mLogger.fine("EmailConfiguration.initialize() completed successfully.");
    }

    /**
     * Initialize the EmailConfiguration instance with the Properties
     * bundle.
     *
     * @param   originalConfig  The Properties bundle containing the
     *                          configuration information.
     */
//     public void initialize(ConfigurationHelper aConfiguration)
//             throws EmailApplicationException {
//         String temp = null;
//         Long numTemp = null;

//         this.emailConfig.clear();

//         /**
//          * Required by JavaMail in order to determine the message delivery
//          * protocol for the session.
//          */
//         this.emailConfig.setProperty("mail.transport.protocol", "smtp");

//         aConfiguration.setSubSection("Connection_Settings");
//         setHostSend(aConfiguration.getStringParameterValue("HostSend", ""));
//         setPortSend(aConfiguration.getIntParameterValue("PortSend", 25));
//         setUserSend(aConfiguration.getStringParameterValue("UserSend", ""));

//         try {
//             setPasswordSend(aConfiguration.getStringParameterValue("PasswordSend", ""));
//         }
//         catch (Exception ex) {
//             String str =
//                 "EmailConfiguration.initialize(): " +
//                 "Wrong UserSend=" + getUserSend() + " or PasswordSend=" + temp + ",\n" +
//                 "Exception info: " + ex.toString();
//             mLogger.error (str, ex);
//             throw new EmailApplicationException (str);
//         }

//         /**
//          * Required by JavaMail in order to determine the message retrieval
//          * protocol for the session.
//          */
//         this.emailConfig.setProperty("mail.store.protocol", "pop3");

//         setHostRecv(aConfiguration.getStringParameterValue("HostRecv", ""));
//         setPortRecv(aConfiguration.getIntParameterValue("PortRecv", 110));
//         setUserRecv(aConfiguration.getStringParameterValue("UserRecv", ""));
//         try {
//             setPasswordRecv(aConfiguration.getStringParameterValue("PasswordRecv", ""));
//         }
//         catch (Exception ex) {
//             String str =
//                 "EmailConfiguration.initialize(): " +
//                 "Wrong UserRecv=" + getUserRecv() + " or PasswordRecv=" + temp + ",\n" +
//                 "Exception info:" + ex.toString();
//             mLogger.error (str, ex);
//             throw new EmailApplicationException (str);
//         }

//         /**
//          * Determine whether to perform POP3 authentication before attempting
//          * SMTP transfer.
//          */
//         setSessionAuth(aConfiguration.getStringParameterValue("SessionAuth", "NO").equalsIgnoreCase("YES"));

//         /**
//          * Prepare for SSL
//          */
//         aConfiguration.setSubSection("SSL");
//         try {
//             boolean recvSSL = aConfiguration.getStringParameterValue("Receive/UseSSL", "NO").equalsIgnoreCase("YES");
//             boolean sendSSL = aConfiguration.getStringParameterValue("Send/UseSSL", "NO").equalsIgnoreCase("YES");

//             // Set up the TrustStore
//             if (recvSSL || sendSSL) {

//                 if (mLogger.isDebugEnabled()) {
//                     System.setProperty("javax.net.debug", "all");
//                 }

//                 // Set up the trustore
//                 aConfiguration.setSubSection("SSL/CACerts");
//                 String trustStore = aConfiguration.getStringParameterValue("TrustStore", null);
//                 if (trustStore == null || trustStore.length() == 0) {
//                     String str =
//                         "EmailConfiguration.initialize(): " +
//                         "TrustStore is not specified in the configuration.";
//                     mLogger.error (str);
//                     throw new EmailApplicationException (str);
//                 }
//                 String trustStoreType = aConfiguration.getStringParameterValue("TrustStoreType", null);
//                 if (trustStoreType == null || trustStoreType.length() == 0) {
//                     String str =
//                         "EmailConfiguration.initialize(): " +
//                         "TrustStoreType is not specified in the configuration.";
//                     mLogger.error (str);
//                     throw new EmailApplicationException (str);
//                 }
//                 String trustStorePass = aConfiguration.getStringParameterValue("TrustStorePassword", null);
//                 if (trustStorePass == null || trustStorePass.length() == 0) {
//                     String str =
//                         "EmailConfiguration.initialize(): " +
//                         "TrustStorePassword is not specified in the configuration.";
//                     mLogger.error (str);
//                     throw new EmailApplicationException (str);
//                 }
//                 mLogger.info ("Using TrustStore " + trustStore + " of type " + trustStoreType);
//                 EmailSystemProperties.setTrustStore(trustStore);
//                 EmailSystemProperties.setTrustStoreType(trustStoreType);
//                 EmailSystemProperties.setTrustStorePassword(trustStorePass);

//                 // Setup receive
//                 if (recvSSL) {
//                     mLogger.info ("SSL is enabled for RECEIVE; using " + SSL_FACTORY + " for creating ssl sockets.");
//                     // POP3 provider
//                     this.emailConfig.setProperty( "mail.pop3.socketFactory.class", SSL_FACTORY);

//                     // POP3 provider - no fallback.
//                     this.emailConfig.setProperty( "mail.pop3.socketFactory.fallback", "false");
//                 } else {
//                     mLogger.debug ("SSL is NOT enabled for RECEIVE.");
//                 }

//                 // Setup send
//                 if (sendSSL) {
//                     mLogger.info ("SSL is enabled for SEND; using " + SSL_FACTORY + " for creating ssl sockets.");
//                     // SMTP provider
//                     this.emailConfig.setProperty( "mail.smtp.socketFactory.class", SSL_FACTORY);

//                     // SMTP provider - no fallback.
//                     this.emailConfig.setProperty( "mail.smtp.socketFactory.fallback", "false");
//                 } else {
//                     mLogger.debug ("SSL is NOT enabled for SEND.");
//                 }

//             } else {
//                 mLogger.debug ("SSL is NOT enabled.");
//             }
//         } catch (Exception ex) {
//             mLogger.error ("EmailConfiguration.initialize() failed due to an exception", ex);
//             throw new EmailApplicationException ("EmailConfiguration.initialize() failed due to an exception; " + ex.toString());
//         }
//         mLogger.debug ("EmailConfiguration.initialize() completed successfully.");
//     }

    /**
     * Returns the contained <code>Properties</code> object for use during session
     * construction.
     */
    protected Properties getProperties() {
        return emailConfig;
    }

    /**
     * Returns new <code>Authenticator</code> for use during session construction.
     */
    protected Authenticator getAuthenticator() {
        return new EmailAuthenticator(this);
    }

    /**
     * End user properties exposed through the GUI follow after this point.
     */

    public String getHostSend() {
        return emailConfig.getProperty("mail.smtp.host", "");
    }
    /**
     * 
     * @param val 
     */
    public void setHostSend(final String val) {
        emailConfig.setProperty("mail.smtp.host", val);
        mLogger.fine ("HostSend set to " + val);
    }

    /**
     * 
     * @return 
     */
    public String getHostRecv() {
        return emailConfig.getProperty("mail.pop3.host", "");
    }
    /**
     * 
     * @param val 
     */
    public void setHostRecv(final String val) {
        emailConfig.setProperty("mail.pop3.host", val);
        mLogger.fine ("HostRecv set to " + val);
    }

    /**
     * 
     * @return 
     */
    public int getPortSend() {
        return Integer.valueOf(emailConfig.getProperty("mail.smtp.port", "0")).intValue();
    }
    
    public int getPortSendSSL(){
        return Integer.valueOf(emailConfig.getProperty(MAIL_FACTORY_PORT, "0")).intValue();
    }
    /**
     * 
     * @param val 
     */
    public void setPortSend(final int val) {
        emailConfig.setProperty("mail.smtp.port", String.valueOf(val));
        mLogger.fine ("PortSend set to " + val);
    }
    
    public void setPortSendSSL(final int val){
        emailConfig.setProperty(MAIL_FACTORY_PORT, String.valueOf(val));
        mLogger.fine ("PortSend set to ssl enabled  " + val);
    }
    //emailConfig.setProperty(MAIL_FACTORY_CLASS , javax.net.ssl.SSLSocketFactory);
    /**
     * 
     * @return 
     */
    public int getPortRecv() {
        return Integer.valueOf(emailConfig.getProperty("mail.pop3.port", "0")).intValue();
    }
    /**
     * 
     * @param val 
     */
    public void setPortRecv(final int val) {
        emailConfig.setProperty("mail.pop3.port", String.valueOf(val));
        mLogger.fine ("PortRecv set to " + val);
    }

    /**
     * The follwing two parameters are used also by the EmailAuthenticator
     * instance to provide session authentication.
     */
    private String  _UserSend;
    /**
     * 
     * @return 
     */
    public String getUserSend() {
      return _UserSend;
    }
    /**
     * 
     * @param val 
     */
    public void setUserSend(final String val) {
      _UserSend = val;
      if ((_UserSend != null) && (_UserSend.length() > 0)) {
          emailConfig.setProperty("mail.smtp.auth", String.valueOf(true));
      }
      else {
          emailConfig.setProperty("mail.smtp.auth", String.valueOf(false));
      }
      mLogger.fine ("UserSend set to " + _UserSend);
    }

    private String  _UserRecv;
    /**
     * 
     * @return 
     */
    public String getUserRecv() {
      return _UserRecv;
    }
    /**
     * 
     * @param val 
     */
    public void setUserRecv(final String val) {
      _UserRecv = val;
      mLogger.fine ("UserRecv set to " + _UserRecv);
    }

    private String  _PasswordSend;
    /**
     * 
     * @return 
     */
    public String getPasswordSend() {
      return _PasswordSend;
    }
    /**
     * 
     * @param val 
     */
    public void setPasswordSend(final String val) {
      _PasswordSend = val;
    }

    private String  _PasswordRecv;
    /**
     * 
     * @return 
     */
    public String getPasswordRecv() {
      return _PasswordRecv;
    }
    /**
     * 
     * @param val 
     */
    public void setPasswordRecv(final String val) {
      _PasswordRecv = val;
    }

    /**
     * This parameter determines whether a POP3 authentication will be performed
     * before attempting an SMTP transfer.
     */
    private boolean _SessionAuth;
    /**
     * 
     * @return 
     */
    public boolean getSessionAuth() {
        return _SessionAuth;
    }
    /**
     * 
     * @param val 
     */
    public void setSessionAuth(final boolean val) {
        _SessionAuth = val;
        mLogger.fine ("SessionAuth set to " + _SessionAuth);
    }
    
    private boolean _useSSL;
    
     /**
      * 
      * @param val 
      */
     public void setUseSSL(final boolean val) {
      _useSSL = val;
      if (_useSSL) {
          emailConfig.setProperty("mail.smtp.auth", String.valueOf(true));
          //emailConfig.setProperty(MAIL_FACTORY_PORT, "465");
          emailConfig.setProperty(MAIL_FACTORY_FALLBACK, "false");
          emailConfig.setProperty(MAIL_FACTORY_CLASS,"javax.net.ssl.SSLSocketFactory");
          emailConfig.setProperty("mail.store.protocol", "imap");
                         
      }
      else {
          emailConfig.setProperty("mail.smtp.auth", String.valueOf(false));
      }
      mLogger.fine ("UserSend set to " + _UserSend);
    }

    public boolean getUseSSL(){
        return _useSSL;
    }
    private class EmailAuthenticator extends Authenticator {
        private EmailConfiguration configuration;

        /** Creates new ConstAuthenticator */
        public EmailAuthenticator(final EmailConfiguration configuration) {
            this.configuration = configuration;
        }

        @Override
		protected PasswordAuthentication getPasswordAuthentication() {
            if (getRequestingProtocol().equalsIgnoreCase("smtp")) {
                mLogger.fine ("smtp protocol authentication; userSend [" + configuration.getUserSend() + "]");
                return new PasswordAuthentication(configuration.getUserSend(), configuration.getPasswordSend());
            }
            else if (getRequestingProtocol().equalsIgnoreCase("pop3")) {
                mLogger.fine ("pop3 protocol authentication; userRecv [" + configuration.getUserRecv() + "]");
                return new PasswordAuthentication(configuration.getUserRecv(), configuration.getPasswordRecv());
            }
            else {
                return null;
            }
        }
    }
}
