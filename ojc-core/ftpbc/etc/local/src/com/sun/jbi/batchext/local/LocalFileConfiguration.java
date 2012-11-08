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
 * @(#)LocalFileConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/******************************************************************************
 * Copyright © 2006 Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * California 95054, U.S.A. All rights reserved. Sun Microsystems, Inc. has
 * intellectual property rights relating to technology embodied in the product
 * that is described in this document. In particular, and without limitation,
 * these intellectual property rights may include one or more of the U.S. patents
 * listed at http://www.sun.com/patents and one or more additional patents or
 * pending patent applications in the U.S. and in other countries. THIS PRODUCT
 * CONTAINS CONFIDENTIAL INFORMATION AND TRADE SECRETS OF SUN MICROSYSTEMS, INC.
 * USE, DISCLOSURE OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN
 * PERMISSION OF SUN MICROSYSTEMS, INC.U.S. Government Rights - Commercial
 * software.  Government users are subject to the Sun Microsystems, Inc. standard
 * license agreement and applicable provisions of the FAR and its supplements.
 * Use is subject to license terms.  This distribution may include materials
 * developed by third parties. Sun, Sun Microsystems, the Sun logo, Java
 * Composite Application Platform Suite,  SeeBeyond, eGate, eInsight, eVision, eTL,
 * eXchange, eView, eIndex, eBAM and  eWay are trademarks or registered trademarks of
 * Sun Microsystems, Inc. in the U.S. and other countries. All SPARC trademarks are
 * used under license and are trademarks or registered trademarks of SPARC
 * International, Inc. in the U.S. and other countries. Products bearing SPARC
 * trademarks are based upon architecture developed by Sun Microsystems, Inc.
 * UNIX is a registered trademark in the U.S. and other countries, exclusively
 * licensed through X/Open Company, Ltd. This product is covered and controlled by
 * U.S. Export Control laws and may be subject to the export or import laws in
 * other countries.  Nuclear, missile, chemical biological weapons or nuclear
 * maritime end uses or end users, whether direct or indirect, are strictly
 * prohibited.  Export or reexport to countries subject to U.S. embargo or to
 * entities identified on U.S. export exclusion lists, including, but not limited
 * to, the denied persons and specially designated nationals lists is strictly
 * prohibited.
 **/
package com.sun.jbi.batchext.local;

import com.sun.jbi.batchext.BatchException;
import java.io.File;
import java.io.FileInputStream;
import java.util.Properties;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

/**
 * This class represents the local file configuration.
 */
public class LocalFileConfiguration {
    //private static final Messages mMessages =
    //        Messages.getMessages(LocalFileConfiguration.class);
    private static final Logger mLogger =
            Messages.getLogger(LocalFileConfiguration.class);
    
    private ConfigChangeListener mConfigChangeListener;
    public static final String PRETC_UNDEFINED = "Undefined";
    public static final String PRETC_NONE = "None";
    public static final String PRETC_RENAME = "Rename";
    public static final String PRETC_COPY = "Copy";
    
    public static final String TC_UNDEFINED = "Undefined";
    public static final String TC_GET = "Get";
    public static final String TC_PUT = "Put";
    
    public static final String POSTTC_UNDEFINED = "Undefined";
    public static final String POSTTC_NONE = "None";
    public static final String POSTTC_RENAME = "Rename";
    public static final String POSTTC_DELETE = "Delete";
    public static final String POSTTC_COPY = "Copy";
    
    public static final String TT_NON_TRANSACTIONAL = "Non-Transactional";
    public static final String TT_XA_COMPLIANT = "XA-compliant";
    
    public static final String DYNCFG_MALFORMED_ACTION_EXIT = "Exit";
    public static final String DYNCFG_MALFORMED_ACTION_IGNORE = "Ignore";
    public static final String DYNCFG_MALFORMED_ACTION_RAISE_ALERT = "Raise alert";
    public static final String DYNCFG_MALFORMED_ACTION_PUBLISH_ERROR = "Publish error record";
    
    private Properties config;
    private boolean bSynchronized;
    
//  private LocalFileDynConfigSection mDynSec;
    
    public LocalFileConfiguration() {
    }
    
    public void initialize(Properties props) throws BatchException {
        this.config = props;
        reset();
    }
    
    /**
     * Resets the configuration at run time by loading the defaults from the
     * e*Way connection configuration file.
     */
    public void reset() throws BatchException {
        // General Settings Section
        _TransactionType = config.getProperty(LocalFileConfigConstants.TransactionType_KEY, TT_NON_TRANSACTIONAL);
        _ResumeReadingEnabled = config.getProperty(LocalFileConfigConstants.ResumeReadingEnabled_KEY, "").equalsIgnoreCase("YES");
        _StatePersistenceBaseLocation = config.getProperty(LocalFileConfigConstants.StatePersistenceBaseLocation_KEY, "");
        this.bSynchronized = config.getProperty(LocalFileConfigConstants.Synchronized_KEY, "Yes").equalsIgnoreCase("Yes");
        // Target Location Section
        _TargetDirectoryName = config.getProperty(LocalFileConfigConstants.TargetDirectoryName_KEY, "");
        
        _TargetDirectoryNameIsPattern = config.getProperty(LocalFileConfigConstants.TargetDirectoryNameIsPattern_KEY, "").equalsIgnoreCase("YES");
        _TargetFileName = config.getProperty(LocalFileConfigConstants.TargetFileName_KEY, "");
        _TargetFileNameIsPattern = config.getProperty(LocalFileConfigConstants.TargetFileNameIsPattern_KEY, "").equalsIgnoreCase("YES");
        _Append = config.getProperty(LocalFileConfigConstants.Append_KEY, "No").equalsIgnoreCase("YES");
        // Pre Transfer Section
        _PreTransferCommand = config.getProperty(LocalFileConfigConstants.PreTransferCommand_KEY, PRETC_UNDEFINED);
        _PreDirectoryName = config.getProperty(LocalFileConfigConstants.PreDirectoryName_KEY, "");
        _PreDirectoryNameIsPattern = config.getProperty(LocalFileConfigConstants.PreDirectoryNameIsPattern_KEY, "").equalsIgnoreCase("YES");
        _PreFileName = config.getProperty(LocalFileConfigConstants.PreFileName_KEY, "");
        _PreFileNameIsPattern = config.getProperty(LocalFileConfigConstants.PreFileNameIsPattern_KEY, "").equalsIgnoreCase("YES");
        // Post Transfer Section
        _PostTransferCommand = config.getProperty(LocalFileConfigConstants.PostTransferCommand_KEY, POSTTC_UNDEFINED);
        _PostDirectoryName = config.getProperty(LocalFileConfigConstants.PostDirectoryName_KEY, "");
        _PostDirectoryNameIsPattern = config.getProperty(LocalFileConfigConstants.PostDirectoryNameIsPattern_KEY, "").equalsIgnoreCase("YES");
        _PostFileName = config.getProperty(LocalFileConfigConstants.PostFileName_KEY, "");
        _PostFileNameIsPattern = config.getProperty(LocalFileConfigConstants.PostFileNameIsPattern_KEY, "").equalsIgnoreCase("YES");
        // Sequence Numbering Section
        _StartingSequenceNumber = getNumberValue(config, LocalFileConfigConstants.StartingSequenceNumber_KEY, 0);
        _MaxSequenceNumber = getNumberValue(config, LocalFileConfigConstants.MaxSequenceNumber_KEY, 999999);
        // Dynamic Configuration Section
        _publishStatusRecordOnSuccess = config.getProperty(LocalFileConfigConstants.PublishStatusRecordOnSuccess_KEY, "No").equalsIgnoreCase("YES");
        _publishStatusRecordOnError = config.getProperty(LocalFileConfigConstants.PublishStatusRecordOnError_KEY, "No").equalsIgnoreCase("YES");
        _includeOrderRecordInErrorRecord = config.getProperty(LocalFileConfigConstants.IncludeOrderRecordInErrorRecord_KEY, "No").equalsIgnoreCase("YES");
        _includePayloadInErrorRecord = config.getProperty(LocalFileConfigConstants.IncludePayloadInErrorRecord_KEY, "No").equalsIgnoreCase("YES");
        _actionOnMalformedCommand = config.getProperty(LocalFileConfigConstants.ActionOnMalformedCommand_KEY, "Exit");
        
        // Extensions Section
        _ClientClassName = config.getProperty(LocalFileConfigConstants.ClientClassName_KEY, "");
        _ProviderClassName = config.getProperty(LocalFileConfigConstants.ProviderClassName_KEY, "");
        _ExtensionPropertiesFile = config.getProperty(LocalFileConfigConstants.ExtensionPropertiesFile_KEY, "");
        if ((_ClientClassName.length() != 0)
        || (_ProviderClassName.length() != 0)
        || (_ExtensionPropertiesFile.length() != 0)) {
            // Attempt to use extensibility features.
            String msg = "LocalFileConfiguration.reset: An attempt to specify a custom Client or Provider implementation was detected. This version of the BatchLocal does not support user extensibility.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new BatchException(msg);
        }
        _ClientClassName = com.sun.jbi.batchext.local.LocalFileClientImpl.class.getName();
        _ProviderClassName = com.sun.jbi.batchext.local.LocalFileProviderImpl.class.getName();
        _ExtensionProperties = new Properties();
        if (_ExtensionPropertiesFile.length() != 0) {
            File pf = new File(_ExtensionPropertiesFile);
            try {
                // Attempt to load the file
                FileInputStream is = new FileInputStream(pf);
                _ExtensionProperties.load(is);
            } catch (Exception ex) {
                String msg = "LocalFileConfiguration.reset: LocalFileConfiguration: Can not load extension properties file "
                        + _ExtensionPropertiesFile;
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new BatchException(msg, ex);
            }
        }
        
        // Validate operation independent setttings
        validateBasics();
    }
    
    protected String getOID() {
        return this.getProperty("conn-props.collaboration.oid");
    }
    
    protected String getExternalName() {
        return this.getProperty("conn-props.connection.name");
    }
    
    private String getProperty(String key) {
        return this.config.getProperty(key);
    }
    
    // This one validates some basics, which do not depend on the transfer command
    protected void validateBasics() throws BatchException {
        // return false instead of throw below
        if (!(_TransactionType.equalsIgnoreCase(TT_NON_TRANSACTIONAL)
        || _TransactionType.equalsIgnoreCase(TT_XA_COMPLIANT))) {
            String msg = "LocalFileConfiguration.validateBasics: Invalid transaction type.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new BatchException(msg);
        }
        if (!(_PreTransferCommand.equalsIgnoreCase(PRETC_RENAME)
        || _PreTransferCommand.equalsIgnoreCase(PRETC_COPY)
        || _PreTransferCommand.equalsIgnoreCase(PRETC_NONE))) {
            String msg = "LocalFileConfiguration.validateBasics: Invalid pre transfer command "
                    + _PreTransferCommand;
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new BatchException(msg);
        }
        if ((_PreTransferCommand.equalsIgnoreCase(PRETC_RENAME)
        || _PreTransferCommand.equalsIgnoreCase(PRETC_COPY))
        && ((_PreDirectoryName.length() == 0)
        || (_PreFileName.length() == 0))) {
            String msg = "LocalFileConfiguration.validateBasics: The pre transfer directory or file name is missing.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new BatchException(msg);
        }
        if (!(_PostTransferCommand.equalsIgnoreCase(POSTTC_RENAME)
        || _PostTransferCommand.equalsIgnoreCase(POSTTC_DELETE)
        || _PostTransferCommand.equalsIgnoreCase(POSTTC_COPY)
        || _PostTransferCommand.equalsIgnoreCase(POSTTC_NONE))) {
            String msg = "LocalFileConfiguration.validateBasics: Invalid post transfer command "
                    +_PostTransferCommand;
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new BatchException(msg);
        }
        if ( (_PostTransferCommand.equalsIgnoreCase(POSTTC_RENAME)
        || _PostTransferCommand.equalsIgnoreCase(POSTTC_COPY))
        && ((_PostDirectoryName.length() == 0)
        || (_PostFileName.length() == 0))) {
            String msg = "LocalFileConfiguration.validateBasics: The post transfer directory or file name is missing.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new BatchException(msg);
        }
        if (_StartingSequenceNumber >= _MaxSequenceNumber) {
            String msg = "LocalFileConfiguration.validateBasics: Invalid sequence numbers. Start should be less than Max.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new BatchException(msg);
        }
        if (!(_actionOnMalformedCommand.equalsIgnoreCase(DYNCFG_MALFORMED_ACTION_EXIT)
        || _actionOnMalformedCommand.equalsIgnoreCase(DYNCFG_MALFORMED_ACTION_IGNORE)
        || _actionOnMalformedCommand.equalsIgnoreCase(DYNCFG_MALFORMED_ACTION_RAISE_ALERT)
        || _actionOnMalformedCommand.equalsIgnoreCase(DYNCFG_MALFORMED_ACTION_PUBLISH_ERROR))) {
            String msg = "LocalFileConfiguration.validateBasics: Invalid action for Malformed Command: " + _actionOnMalformedCommand;
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new BatchException(msg);
        }
        
        if (_ClientClassName.length() == 0) {
            String msg = "LocalFileConfiguration.validateBasics: Missing client class name.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new BatchException(msg);
        }
        if (_ProviderClassName.length() == 0) {
            String msg = "LocalFileConfiguration.validateBasics: Missing provider class name.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new BatchException(msg);
        }
    }
    
// General Settings Section
    // Transaction Type
    //private static final String TransactionType_KEY = "General Settings/Transaction Type";
    private String _TransactionType = TT_NON_TRANSACTIONAL;
    
    /**
     * Gets the transaction type setting.
     * @return The transaction type.
     */
    public String getTransactionType() {
        return _TransactionType;
    }
    // Resume Reading Enabled
    //private static final String ResumeReadingEnabled_KEY = "General Settings/Resume Reading Enabled";
    private boolean _ResumeReadingEnabled = false;
    /**
     * Gets the resume reading enabled setting.
     * @return true if enabled, false if disabled.
     */
    public boolean getResumeReadingEnabled() {
        return _ResumeReadingEnabled;
    }
    /**
     * Sets the resume reading enabled setting.
     * @param resumeReadingEnabled true to enable, false to disable.
     */
    public void setResumeReadingEnabled(boolean resumeReadingEnabled) {
        _ResumeReadingEnabled = resumeReadingEnabled;
    }
    // State Persistence Base Location
    //private static final String StatePersistenceBaseLocation_KEY = "General Settings/State Persistence Base Location";
    private String _StatePersistenceBaseLocation = "";
    /**
     * Gets the state persistence base location setting.
     * @return The state persistence base location
     */
    public String getStatePersistenceBaseLocation() {
        return _StatePersistenceBaseLocation;
    }
    
// Target Location Section
    // Contains the target directory name or a pattern to expand
    //private static final String TargetDirectoryName_KEY = "Target Location/Target Directory Name";
    private String _TargetDirectoryName = "";
    /**
     * Gets the target directory name setting
     * @return The target directory name.
     */
    public String getTargetDirectoryName() {
        return _TargetDirectoryName;
    }
    /**
     * Gets the target directory name setting
     * @param targetDirectoryName The new target directory name.
     */
    public void setTargetDirectoryName(String targetDirectoryName) throws Exception {
        _TargetDirectoryName = targetDirectoryName;
        if ( this.mConfigChangeListener != null ) {
            // this is a dynamic set from collab
            // check the validity of dynamic setting
            if ( this.getResumeReadingEnabled() )
                throw new Exception("Error : Dynamic setting of TargetDirectoryName in the collaboration is not allowed while resume reading is on.");
        }
        this.fireChangeEvent();
    }
    
    // true – the Target Directory Name is pattern
    // false – the Target Directory Name is literal
    private boolean _TargetDirectoryNameIsPattern = false;
    /**
     * Gets the target directory name is pattern setting.
     * @return true if pattern, false if literal.
     */
    public boolean getTargetDirectoryNameIsPattern() {
        return _TargetDirectoryNameIsPattern;
    }
    /**
     * Sets the target directory name is pattern setting.
     * @param targetDirectoryNameIsPattern true to set as pattern, false to set as literal.
     */
    public void setTargetDirectoryNameIsPattern(boolean targetDirectoryNameIsPattern) throws Exception {
        _TargetDirectoryNameIsPattern = targetDirectoryNameIsPattern;
        if ( this.mConfigChangeListener != null ) {
            // this is a dynamic set from collab
            // check the validity of dynamic setting
            if ( this.getResumeReadingEnabled() )
                throw new Exception("Error : Dynamic setting of TargetDirectoryNameIsPattern in the collaboration is not allowed while resume reading is on.");
        }
        this.fireChangeEvent();
    }
    
    // Contains the target file name or a pattern to expand
    private String _TargetFileName = "";
    /**
     * Gets the target file name setting.
     * @return The target file name.
     */
    public String getTargetFileName() {
        return _TargetFileName;
    }
    /**
     * Sets the target file name setting.
     * @param targetFileName The new target file name.
     */
    public void setTargetFileName(String targetFileName) throws Exception {
        _TargetFileName = targetFileName;
        if ( this.mConfigChangeListener != null ) {
            // this is a dynamic set from collab
            // check the validity of dynamic setting
            if ( this.getResumeReadingEnabled() )
                throw new Exception("Error : Dynamic setting of TargetFileName in the collaboration is not allowed while resume reading is on.");
        }
        this.fireChangeEvent();
    }
    
    // true – the Target File Name is pattern
    // false – the Target File Name is literal
    private boolean _TargetFileNameIsPattern = false;
    /**
     * Gets the target file name is pattern setting.
     * @return true if pattern, false if literal.
     */
    public boolean getTargetFileNameIsPattern() {
        return _TargetFileNameIsPattern;
    }
    /**
     * Sets the target file name is pattern setting.
     * @param targetFileNameIsPattern true to set as pattern, false to set as literal.
     */
    public void setTargetFileNameIsPattern(boolean targetFileNameIsPattern) throws Exception {
        _TargetFileNameIsPattern = targetFileNameIsPattern;
        if ( this.mConfigChangeListener != null ) {
            // this is a dynamic set from collab
            // check the validity of dynamic setting
            if ( this.getResumeReadingEnabled() )
                throw new Exception("Error : Dynamic setting of TargetFileNameIsPattern in the collaboration is not allowed while resume reading is on.");
        }
        fireChangeEvent();
    }
    
    // true – append mode
    // false – overwrite mode
    private boolean _Append = false;
    /**
     * Gets the append mode setting.
     * @return true if append, false if overwrite.
     */
    public boolean getAppend() {
        return _Append;
    }
    /**
     * Sets the append mode setting.
     * @param append true to set for append, false to set for overwrite.
     */
    public void setAppend(boolean append) {
        _Append = append;
    }
    
// Pre Transfer Section
    private String _PreTransferCommand = PRETC_UNDEFINED;
    /**
     * Gets the pre transfer command setting.
     * @return The pre transfer command.
     */
    public String getPreTransferCommand() {
        return _PreTransferCommand;
    }
    /**
     * Sets the pre command setting.
     * @param preTransferCommand The new pre command.
     */
    public void setPreTransferCommand(String preTransferCommand) {
        _PreTransferCommand = preTransferCommand;
    }
    
    // Contains the new directory name (Rename/Copy command).
    private String _PreDirectoryName;
    /**
     * Gets the pre directory name.
     * @return The pre directory name.
     */
    public String getPreDirectoryName() {
        return _PreDirectoryName;
    }
    /**
     * Sets the pre directory name.
     * @param preDirectoryName The new pre directory name.
     */
    public void setPreDirectoryName(String preDirectoryName) {
        _PreDirectoryName = preDirectoryName;
        this.fireChangeEvent();
    }
    
    // true – the Pre Transfer Directory Name is pattern
    // false – the Pre Transfer Directory Name is literal
    private boolean _PreDirectoryNameIsPattern = false;
    /**
     * Gets the pre directory name is pattern setting.
     * @return true if pattern, false if literal.
     */
    public boolean getPreDirectoryNameIsPattern() {
        return _PreDirectoryNameIsPattern;
    }
    /**
     * Sets the pre directory name is pattern setting.
     * @param preDirectoryNameIsPattern true to set as pattern, false to set as literal.
     */
    public void setPreDirectoryNameIsPattern(boolean preDirectoryNameIsPattern) {
        _PreDirectoryNameIsPattern = preDirectoryNameIsPattern;
        this.fireChangeEvent();
    }
    
    // Contains the new file name (Rename/Copy command)
    private String _PreFileName;
    /**
     * Gets the pre file name setting.
     * @return The pre file name.
     */
    public String getPreFileName() {
        return _PreFileName;
    }
    /**
     * Sets the pre file name setting.
     * @param preFileName The new pre file name.
     */
    public void setPreFileName(String preFileName) {
        _PreFileName = preFileName;
        this.fireChangeEvent();
    }
    
    // true – the Pre Transfer File Name is pattern
    // false – the Pre Transfer File Name is literal
    private boolean _PreFileNameIsPattern = false;
    /**
     * Gets the pre file name is pattern setting.
     * @return true if pattern, false if literal.
     */
    public boolean getPreFileNameIsPattern() {
        return _PreFileNameIsPattern;
    }
    /**
     * Sets the pre file name is pattern setting.
     * @param preFileNameIsPattern true to set as pattern, flase to set as literal.
     */
    public void setPreFileNameIsPattern(boolean preFileNameIsPattern) {
        _PreFileNameIsPattern = preFileNameIsPattern;
        this.fireChangeEvent();
    }
    
// Post Transfer Section
    private String _PostTransferCommand = POSTTC_UNDEFINED;
    /**
     * Gets the post transfer command setting.
     * @return The post transfer command.
     */
    public String getPostTransferCommand() {
        return _PostTransferCommand;
    }
    /**
     * Sets the post transfer command setting.
     * @param postTransferCommand The new post transfer command.
     */
    public void setPostTransferCommand(String postTransferCommand) {
        _PostTransferCommand = postTransferCommand;
    }
    
    // Contains the new directory name (Rename command).
    private String _PostDirectoryName;
    /**
     * Gets the post directory name setting.
     * @return The post directory name.
     */
    public String getPostDirectoryName() {
        return _PostDirectoryName;
    }
    /**
     * Sets the post directory name setting.
     * @param PostDirectoryName The new post directory name.
     */
    public void setPostDirectoryName(String PostDirectoryName) {
        _PostDirectoryName = PostDirectoryName;
        this.fireChangeEvent();
    }
    
    // true – the Post Transfer Directory Name is pattern
    // false – the Post Transfer Directory Name is literal
    private boolean _PostDirectoryNameIsPattern = false;
    /**
     * Gets the post directory name is pattern setting.
     * @return true if pattern, else if literal.
     */
    public boolean getPostDirectoryNameIsPattern() {
        return _PostDirectoryNameIsPattern;
    }
    /**
     * Sets the post directory name is pattern setting.
     * @param PostDirectoryNameIsPattern true to set as pattern, false to set as literal.
     */
    public void setPostDirectoryNameIsPattern(boolean PostDirectoryNameIsPattern) {
        _PostDirectoryNameIsPattern = PostDirectoryNameIsPattern;
        this.fireChangeEvent();
    }
    
    // Contains the new file name (Rename command)
    private String _PostFileName;
    /**
     * Gets the post file name setting.
     * @return The post file name.
     */
    public String getPostFileName() {
        return _PostFileName;
    }
    /**
     * Sets the post file name setting.
     * @param PostFileName The new post file name.
     */
    public void setPostFileName(String PostFileName) {
        _PostFileName = PostFileName;
        this.fireChangeEvent();
    }
    
    // true – the Post Transfer File Name is pattern
    // false – the Post Transfer File Name is literal
    private boolean _PostFileNameIsPattern = false;
    /**
     * Gets the post file name is pattern setting.
     * @return true if pattern, false if litteral.
     */
    public boolean getPostFileNameIsPattern() {
        return _PostFileNameIsPattern;
    }
    /**
     * Sets the post file name is pattern setting.
     * @param PostFileNameIsPattern true to set as pattern, false to set as literal.
     */
    public void setPostFileNameIsPattern(boolean PostFileNameIsPattern) {
        _PostFileNameIsPattern = PostFileNameIsPattern;
        this.fireChangeEvent();
    }
    
// Sequence Numbering Section
    // The initial sequence number for the first execution or after the
    // Max Sequence Number was reached.
    private int _StartingSequenceNumber = 0;
    /**
     * Gets the starting sequence number setting.
     * @return The starting sequence number.
     */
    public int getStartingSequenceNumber() {
        return _StartingSequenceNumber;
    }
    /**
     * Sets the starting sequence number setting.
     * @param startingSequenceNumber The new starting sequence number.
     */
    public void setStartingSequenceNumber(int startingSequenceNumber) {
        _StartingSequenceNumber = startingSequenceNumber;
    }
    
    // The sequence number is reset to Starting Sequence Number after
    // reaching the value specified in Max Sequence Number.
    private int _MaxSequenceNumber = 0;
    /**
     * Gets the max sequence number setting.
     * @return The max sequence number.
     */
    public int getMaxSequenceNumber() {
        return _MaxSequenceNumber;
    }
    /**
     * Sets the max sequence number setting.
     * @param maxSequenceNumber The new max sequence number.
     */
    public void setMaxSequenceNumber(int maxSequenceNumber) {
        _MaxSequenceNumber = maxSequenceNumber;
    }
    
    // Dynamic Configuration Section
    
    // A helper method used for ETD configuration sub-node.
    public LocalFileConfiguration getDynamicConfiguration() {
        return this;
    }
//  public LocalFileDynConfigSection getDynamicConfiguration() {
//      if ( this.mDynSec == null )
//          this.mDynSec = new LocalFileDynConfigSection(this);
//      return this.mDynSec;
//  }
    
    private boolean _publishStatusRecordOnSuccess = false;
    /**
     * Checks on whether to publish status record on success or not.
     * @return    true or false.
     */
    public boolean getPublishStatusRecordOnSuccess() {
        return _publishStatusRecordOnSuccess;
    }
    /**
     * Sets on whether to publish status record on success or not.
     * @param    newPublishStatusRecordOnSuccess   true or false.
     */
    public void setPublishStatusRecordOnSuccess(boolean newPublishStatusRecordOnSuccess) {
        _publishStatusRecordOnSuccess = newPublishStatusRecordOnSuccess;
    }
    
    private boolean _publishStatusRecordOnError = false;
    /**
     * Checks on whether to publish status record On error or not.
     * @return    true or false.
     */
    public boolean getPublishStatusRecordOnError() {
        return _publishStatusRecordOnError;
    }
    /**
     * Sets on whether to publish status record on error or not.
     * @param    newPublishStatusRecordOnError   true or false.
     */
    public void setPublishStatusRecordOnError(boolean newPublishStatusRecordOnError) {
        _publishStatusRecordOnError = newPublishStatusRecordOnError;
    }
    
    private boolean _includeOrderRecordInErrorRecord = false;
    /**
     * Checks on whether to include order record in error record or not.
     * @return    true or false.
     */
    public boolean getIncludeOrderRecordInErrorRecord() {
        return _includeOrderRecordInErrorRecord;
    }
    /**
     * Sets on whether to include order record in error record or not.
     * @param    newIncludeOrderRecordInErrorRecord   true or false.
     */
    public void setIncludeOrderRecordInErrorRecord(boolean newIncludeOrderRecordInErrorRecord) {
        _includeOrderRecordInErrorRecord = newIncludeOrderRecordInErrorRecord;
    }
    
    private boolean _includePayloadInErrorRecord = false;
    /**
     * Checks on whether to include payload in error record or not.
     * @return    true or false.
     */
    public boolean getIncludePayloadInErrorRecord() {
        return _includePayloadInErrorRecord;
    }
    /**
     * Sets on whether to include payload in error record or not.
     * @param    newIncludePayloadInErrorRecord   true or false.
     */
    public void setIncludePayloadInErrorRecord(boolean newIncludePayloadInErrorRecord) {
        _includePayloadInErrorRecord = newIncludePayloadInErrorRecord;
    }
    
    private String _actionOnMalformedCommand = null;
    /**
     * Gets Action on Malformed Command.
     * @return    The Action on Malformed Command.
     */
    public String getActionOnMalformedCommand() {
        return _actionOnMalformedCommand;
    }
    /**
     * Sets Action on Malformed Command.
     * @param    newActionOnMalformedCommand   The Action on Malformed Command. Valid entries include Exit, Ignore, Raise alert, and Publish error record.
     */
    public void setActionOnMalformedCommand(String newActionOnMalformedCommand) {
        _actionOnMalformedCommand = newActionOnMalformedCommand;
    }
    
// Extensions Section
    // Allows the user to specify a replacement implementation of the
    // high-level file transfer interface.
    private String _ClientClassName = "";
    public String getClientClassName() {
        return _ClientClassName;
    }
    
    // Allows the user to specify a replacement implementation of the
    // low-level provider interface.
    private String _ProviderClassName = "";
    public String getProviderClassName() {
        return _ProviderClassName;
    }
    
    // This Java properties file gets loaded into a java.io.Properties
    // instance. It can be accessed through getUserProperties() method of
    // the ETD configuration.
    private String _ExtensionPropertiesFile = "";
    public String getExtensionPropertiesFile() {
        return _ExtensionPropertiesFile;
    }
    
    private Properties _ExtensionProperties = null;
    public Properties getExtensionProperties() {
        return _ExtensionProperties;
    }
    
    public String toString() {
        StringBuffer sb = new StringBuffer(500);
        sb.append("\n");
        sb.append(this.getClass().getName());
        sb.append("LocalFileConfiguration instance: ---");
        // General Settings Section
        sb.append("\nTransactionType ");
        sb.append(_TransactionType);
        sb.append("\nResumeReadingEnabled ");
        sb.append(_ResumeReadingEnabled);
        sb.append("\nStatePersistenceBaseLocation ");
        sb.append(_StatePersistenceBaseLocation);
        // Target Location Section
        sb.append("\nTargetDirectoryName ");
        sb.append(_TargetDirectoryName);
        sb.append("\nTargetDirectoryNameIsPattern ");
        sb.append(_TargetDirectoryNameIsPattern);
        sb.append("\nTargetFileName ");
        sb.append(_TargetFileName);
        sb.append("\nTargetFileNameIsPattern ");
        sb.append(_TargetFileNameIsPattern);
        sb.append("\nAppend ");
        sb.append(_Append);
        // Pre Transfer Command Section
        sb.append("\nPreTransferCommand ");
        sb.append(_PreTransferCommand);
        sb.append("\nPreDirectoryName ");
        sb.append(_PreDirectoryName);
        sb.append("\nPreDirectoryNameIsPattern ");
        sb.append(_PreDirectoryNameIsPattern);
        sb.append("\nPreFileName ");
        sb.append(_PreFileName);
        sb.append("\nPreFileNameIsPattern ");
        sb.append(_PreFileNameIsPattern);
        // Post Transfer Command Section
        sb.append("\nPostTransferCommand ");
        sb.append(_PostTransferCommand);
        sb.append("\nPostDirectoryName ");
        sb.append(_PostDirectoryName);
        sb.append("\nPostDirectoryNameIsPattern ");
        sb.append(_PostDirectoryNameIsPattern);
        sb.append("\nPostFileName ");
        sb.append(_PostFileName);
        sb.append("\nPostFileNameIsPattern ");
        sb.append(_PostFileNameIsPattern);
        // Sequence Numbering Section
        sb.append("\nStartingSequenceNumber ");
        sb.append(_StartingSequenceNumber);
        sb.append("\n_MaxSequenceNumber ");
        sb.append(_MaxSequenceNumber);
        // Dynamic Configuration Section
        sb.append("\npublishStatusRecordOnSuccess ");
        sb.append(_publishStatusRecordOnSuccess);
        sb.append("\npublishStatusRecordOnError ");
        sb.append(_publishStatusRecordOnError);
        sb.append("\nincludeOrderRecordInErrorRecord ");
        sb.append(_includeOrderRecordInErrorRecord);
        sb.append("\nincludePayloadInErrorRecord ");
        sb.append(_includePayloadInErrorRecord);
        sb.append("\nactionOnMalformedCommand ");
        sb.append(_actionOnMalformedCommand);
        
        // Extensions Section
        sb.append("\nClientClassName ");
        sb.append(_ClientClassName);
        sb.append("\nProviderClassName ");
        sb.append(_ProviderClassName);
        sb.append("\nExtensionPropertiesFile ");
        sb.append(_ExtensionPropertiesFile);
        sb.append("\nExtensionProperties ");
        sb.append(_ExtensionProperties.toString());
        sb.append("\n---END\n");
        return sb.toString();
    }
    
    // not exposed to collaboration code
    protected void setSynchronized(boolean b) {
        this.bSynchronized=b;
    }
    
    public boolean getSynchronized() {
        return this.bSynchronized;
    }
    
    private int getNumberValue(Properties p, String key, int defValue) {
        Object obj = p.get(key);
        int result = defValue;
        if ( obj != null ) {
            result = Integer.parseInt(obj.toString());
        }
        return result;
    }
    
    void registerConfigChangeListener(ConfigChangeListener l) {
        this.mConfigChangeListener = l;
    }
    
    void unregisterConfigChangeListener(ConfigChangeListener l) {
        this.mConfigChangeListener = null;
    }
    
    private void fireChangeEvent() {
        if ( this.mConfigChangeListener != null )
            this.mConfigChangeListener.notifyChanges();
    }
}
