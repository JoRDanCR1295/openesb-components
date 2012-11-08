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
 * @(#)FtpFileValidator.java 
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
package com.sun.jbi.ftpbc.ftp;

import com.sun.jbi.ftpbc.ftp.exception.FtpFileException;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

/**
 * This class is used to validate the relationships of parameters in the FtpFileConfiguration, that is, the configuration parameters for the FTP ETD.
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */
public class FtpFileValidator {
    private static final Messages mMessages =
            Messages.getMessages(FtpFileValidator.class);
    private static final Logger mLogger =
            Messages.getLogger(FtpFileValidator.class);
    
    private String vMode;
    
    private FtpFileConfiguration configuration = null;
    
    /**
     * Constructor.
     * @param   configuration  An instance of FtpFileConfiguration.
     * @parma            mode  The FTP transfer type ("get" or "put").
     */
    public FtpFileValidator(FtpFileConfiguration configuration, String mode) {
        this.setConfiguration(configuration);
        this.setMode(mode);
    }
    
    /**
     * @return mode - get or put
     */
    public String getMode() {
        return vMode;
    }
    
    /**
     * @param vMode - get or put
     */
    public void setMode(String vMode) {
        this.vMode = vMode;
    }
    
    /**
     * @return object FtpFileConfiguration
     */
    public FtpFileConfiguration getConfiguration() {
        return configuration;
    }
    
    /**
     * @param configuration An object of FtpFileConfiguration
     */
    public void setConfiguration(FtpFileConfiguration configuration) {
        this.configuration = configuration;
    }
    
    public void validate() throws FtpFileException {
        validate(false);
    }
    
    /**
     * Validates against the configuration parameters' relationships.
     *
     * @exception   FtpFileException   If some error occurs.
     */
    // For the single parameter validation, we have
    // done them in property setters. Here is for the
    // relationship validation.
    // These parameters can be set on GUI screen or
    // can be modified in collaboration anytime.
    public void validate(boolean noWarning) throws FtpFileException {
        if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"FtpFileValidator.validate()"}));
        
        String msg = null;
        /* some error messages, throws exception */
        if (this.getConfiguration().getDirectoryListingStyle().equals("MVS GDG")) {
            if (!this.getConfiguration().isPreTransferCommandNone()) {
                msg = mMessages.getString("FTPBC-E006046.ERR_EXT_FTP_NOT_SUPPORTED",
                		new Object[] {
                		"FtpFileValidator.validate()",
                		"PRE Command :" + getConfiguration().getPreTransferCommand(),
                		"MVS GDG"
                });
                if (mLogger.isLoggable(Level.SEVERE))
                    mLogger.log(Level.SEVERE, msg);
                throw new FtpFileException(msg);
            }
            if (!this.getConfiguration().isPostTransferCommandNone()) {
                msg = mMessages.getString("FTPBC-E006046.ERR_EXT_FTP_NOT_SUPPORTED",
                		new Object[] {
                		"FtpFileValidator.validate()",
                		"POST Command :" + getConfiguration().getPostTransferCommand(),
                		"MVS GDG"
                });
                if (mLogger.isLoggable(Level.SEVERE))
                    mLogger.log(Level.SEVERE, msg);
                throw new FtpFileException(msg);
            }
        }
        
        if (this.getConfiguration().isPreTransferCommandCopy()
        || this.getConfiguration().isPreTransferCommandRename()) {
            if (this.getConfiguration().getPreDirectoryName() == null
                    || this.getConfiguration().getPreDirectoryName().equals("")) {
                msg = mMessages.getString("FTPBC-E006021.ERR_EXT_FTP_MISS_OR_BAD_PARAM",
                		new Object[] {
                		"PreDirectoryName"
                		, ""
                });
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new FtpFileException(msg);
            }
            if (this.getConfiguration().getPreFileName() == null
                    || this.getConfiguration().getPreFileName().equals("")) {
                msg = mMessages.getString("FTPBC-E006021.ERR_EXT_FTP_MISS_OR_BAD_PARAM",
                		new Object[] {
                		"PreFileName"
                		, ""
                });
                if (mLogger.isLoggable(Level.SEVERE))
                    mLogger.log(Level.SEVERE, msg);
                throw new FtpFileException(msg);
            }
        }
        
        if (this.getConfiguration().isPostTransferCommandRename()) {
            if (this.getConfiguration().getPostDirectoryName() == null
                    || this.getConfiguration().getPostDirectoryName().equals("")) {
                msg = mMessages.getString("FTPBC-E006021.ERR_EXT_FTP_MISS_OR_BAD_PARAM",
                		new Object[] {
                		"PostDirectoryName"
                		, ""
                });
                if (mLogger.isLoggable(Level.SEVERE))
                    mLogger.log(Level.SEVERE, msg);
                throw new FtpFileException(msg);
            }
            if (this.getConfiguration().getPostFileName() == null
                    || this.getConfiguration().getPostFileName().equals("")) {
                msg = mMessages.getString("FTPBC-E006021.ERR_EXT_FTP_MISS_OR_BAD_PARAM",
                		new Object[] {
                		"PostFileName"
                		, ""
                });
                if (mLogger.isLoggable(Level.SEVERE))
                    mLogger.log(Level.SEVERE, msg);
                throw new FtpFileException(msg);
            }
        }
        
        if (this.getConfiguration().getMaxSequenceNumber() < this.getConfiguration().getStartingSequenceNumber()) {
            msg = mMessages.getString("FTPBC-E006047.ERR_EXT_FTP_INVALID_SEQ_MAX_LT_START",
            		new Object[] {
            		getConfiguration().getMaxSequenceNumber()
            		, getConfiguration().getStartingSequenceNumber()
            });
            if (mLogger.isLoggable(Level.SEVERE))
                mLogger.log(Level.SEVERE, msg);
            throw new FtpFileException(msg);
        }
        
        if (this.getConfiguration().isSocksEnabled() &&
                (this.getConfiguration().getSocksHostName() == null ||
                this.getConfiguration().getSocksHostName().equals(""))) {
            msg = mMessages.getString("FTPBC-E006021.ERR_EXT_FTP_MISS_OR_BAD_PARAM",
            		new Object[] {
            		"SocksHostName"
            		, ""
            });
            if (mLogger.isLoggable(Level.SEVERE))
                mLogger.log(Level.SEVERE, msg);
            throw new FtpFileException(msg);
        }
        
        if (this.getMode().equalsIgnoreCase(FtpFileClient.TC_PUT)) {
            if (this.getConfiguration().isPostTransferCommandDelete()) {
                msg = mMessages.getString("FTPBC-E006021.ERR_EXT_FTP_MISS_OR_BAD_PARAM",
                		new Object[] {
                		"PostTransferCommand"
                		, this.getConfiguration().getPostTransferCommand()
                });
                if (mLogger.isLoggable(Level.SEVERE))
                    mLogger.log(Level.SEVERE, msg);
                throw new FtpFileException(msg);
            }
        }
        
        // some warning messages, no exception is thrown
        if ( !noWarning && mLogger.isLoggable(Level.WARNING)) {
            if (this.getMode().equalsIgnoreCase(FtpFileClient.TC_GET)) {
                if (this.getConfiguration().isPostTransferCommandNone()) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006009.WRN_EXT_FTP_POSTTRANS_IB_NONE", new Object[] {
                    		getConfiguration().getPostTransferCommand()
                    }));
                }
            }
            
            if (this.getMode().equalsIgnoreCase(FtpFileClient.TC_PUT)) {
                // pre 'Rename' but 'Append' mode
                if (this.getConfiguration().isPreTransferCommandRename() &&
                        this.getConfiguration().isAppend()) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006010.WRN_EXT_FTP_PRE_RN_APP", new Object[] {getConfiguration().getPreTransferCommand()}));
                }
                // pre non-'Copy' but 'Append' mode
                if (!this.getConfiguration().isPreTransferCommandCopy() &&
                        this.getConfiguration().isAppend()) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006011.WRN_EXT_FTP_PRE_NON_CPY_APP", new Object[] {getConfiguration().getPreTransferCommand()}));
                }
                // directory name with %f
                if (this.getConfiguration().getTargetDirectoryName().indexOf("%f") >= 0) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006012.WRN_EXT_FTP_PERCENT_F_IN_TGT", new Object[] {"TargetDirectoryName", getConfiguration().getTargetDirectoryName()}));
                }
                // file name with %f
                if (this.getConfiguration().getTargetFileName().indexOf("%f") >= 0) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006012.WRN_EXT_FTP_PERCENT_F_IN_TGT", new Object[] {"TargetFileName", getConfiguration().getTargetFileName()}));
                }
                // directory "Literal" with "%"
                if (!this.getConfiguration().isTargetDirectoryNameIsPattern() &&
                        this.getConfiguration().getTargetDirectoryName().indexOf("%") >= 0) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006013.WRN_EXT_FTP_PERCENT_F_IN_LIT", new Object[] {
                    		"TargetDirectoryName", 
                    		getConfiguration().getTargetDirectoryName(),
                    		"TargetDirectoryNameIsPattern", 
                    		getConfiguration().isTargetDirectoryNameIsPattern()
                    		}));
                }
                // file "Literal" with "%"
                if (!this.getConfiguration().isTargetFileNameIsPattern() &&
                        this.getConfiguration().getTargetFileName().indexOf("%") >= 0) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006013.WRN_EXT_FTP_PERCENT_F_IN_LIT", new Object[] {
                    		"TargetFileName", 
                    		getConfiguration().getTargetFileName(),
                    		"TargetFileNameIsPattern", 
                    		getConfiguration().isTargetFileNameIsPattern()
                    		}));
                }
                // preDirectoryName/preFileName "Literal" with %
                if (this.getConfiguration().isPreTransferCommandCopy() ||
                        this.getConfiguration().isPreTransferCommandRename()) {
                    if (!this.getConfiguration().isPreDirectoryNameIsPattern() &&
                            this.getConfiguration().getPreDirectoryName().indexOf("%") >= 0) {
                        mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006013.WRN_EXT_FTP_PERCENT_F_IN_LIT", new Object[] {
                        		"PreDirectoryName", 
                        		getConfiguration().getPreDirectoryName(),
                        		"PreDirectoryNameIsPattern", 
                        		getConfiguration().isPreDirectoryNameIsPattern()
                        		}));
                    }
                    if (!this.getConfiguration().isPreFileNameIsPattern() &&
                            this.getConfiguration().getPreFileName().indexOf("%") >= 0) {
                        mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006013.WRN_EXT_FTP_PERCENT_F_IN_LIT", new Object[] {
                        		"PreFileName", 
                        		getConfiguration().getPreFileName(),
                        		"PreFileNameIsPattern", 
                        		getConfiguration().isPreFileNameIsPattern()
                        		}));
                    }
                }
                // postDirectoryName/postFileName "Literal" with %
                if (this.getConfiguration().isPostTransferCommandRename()) {
                    if (!this.getConfiguration().isPostDirectoryNameIsPattern() &&
                            this.getConfiguration().getPostDirectoryName().indexOf("%") >= 0) {
                        mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006013.WRN_EXT_FTP_PERCENT_F_IN_LIT", new Object[] {
                        		"PostDirectoryName", 
                        		getConfiguration().getPostDirectoryName(),
                        		"PostDirectoryNameIsPattern", 
                        		getConfiguration().isPostDirectoryNameIsPattern()
                        		}));
                    }
                    if (!this.getConfiguration().isPostFileNameIsPattern() &&
                            this.getConfiguration().getPostFileName().indexOf("%") >= 0) {
                        mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006013.WRN_EXT_FTP_PERCENT_F_IN_LIT", new Object[] {
                        		"PostFileName", 
                        		getConfiguration().getPostFileName(),
                        		"PostFileNameIsPattern", 
                        		getConfiguration().isPostFileNameIsPattern()
                        		}));
                    }
                }
                // directory "Pattern" without "%"
                if (this.getConfiguration().isTargetDirectoryNameIsPattern() &&
                        this.getConfiguration().getTargetDirectoryName().indexOf("%") < 0) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006013.WRN_EXT_FTP_PERCENT_F_IN_LIT", new Object[] {
                    		"TargetDirectoryName", 
                    		getConfiguration().getTargetDirectoryName(),
                    		"TargetDirectoryNameIsPattern", 
                    		getConfiguration().isTargetDirectoryNameIsPattern()
                    		}));
                }
                // file "Pattern" without "%"
                if (this.getConfiguration().isTargetFileNameIsPattern() &&
                        this.getConfiguration().getTargetFileName().indexOf("%") < 0) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006013.WRN_EXT_FTP_PERCENT_F_IN_LIT", new Object[] {
                    		"TargetFileName", 
                    		getConfiguration().getTargetFileName(),
                    		"TargetFileNameIsPattern", 
                    		getConfiguration().isTargetFileNameIsPattern()
                    		}));
                }
                // PreDirectoryName/PreFileName "Pattern" without "%"
                if (this.getConfiguration().isPreTransferCommandCopy() ||
                        this.getConfiguration().isPreTransferCommandRename()) {
                    if (this.getConfiguration().isPreDirectoryNameIsPattern() &&
                            this.getConfiguration().getPreDirectoryName().indexOf("%") < 0) {
                        mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006013.WRN_EXT_FTP_PERCENT_F_IN_LIT", new Object[] {
                        		"PreDirectoryName", 
                        		getConfiguration().getPreDirectoryName(),
                        		"PreDirectoryNameIsPattern", 
                        		getConfiguration().isPreDirectoryNameIsPattern()
                        		}));
                    }
                    if (this.getConfiguration().isPreFileNameIsPattern() &&
                            this.getConfiguration().getPreFileName().indexOf("%") < 0) {
                        mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006013.WRN_EXT_FTP_PERCENT_F_IN_LIT", new Object[] {
                        		"PreFileName", 
                        		getConfiguration().getPreFileName(),
                        		"PreFileNameIsPattern", 
                        		getConfiguration().isPreFileNameIsPattern()
                        		}));
                    }
                }
                // PostDirectoryName/PostFileName "Pattern" without "%"
                if (this.getConfiguration().isPostTransferCommandRename()) {
                    if (this.getConfiguration().isPostDirectoryNameIsPattern() &&
                            this.getConfiguration().getPostDirectoryName().indexOf("%") < 0) {
                        mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006013.WRN_EXT_FTP_PERCENT_F_IN_LIT", new Object[] {
                        		"PostDirectoryName", 
                        		getConfiguration().getPostDirectoryName(),
                        		"PostDirectoryNameIsPattern", 
                        		getConfiguration().isPostDirectoryNameIsPattern()
                        		}));
                    }
                    if (this.getConfiguration().isPostFileNameIsPattern() &&
                            this.getConfiguration().getPostFileName().indexOf("%") < 0) {
                        mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006013.WRN_EXT_FTP_PERCENT_F_IN_LIT", new Object[] {
                        		"PostFileName", 
                        		getConfiguration().getPostFileName(),
                        		"PostFileNameIsPattern", 
                        		getConfiguration().isPostFileNameIsPattern()
                        		}));
                    }
                }
            }
        }
        
        return;
    }
    
}
