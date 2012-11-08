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
 * @(#)BatchRecordConfiguration.java 
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
package com.sun.jbi.batchext.record;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;


/**
 * This class holds the configuration information for the record-processing ETD. These
 * configuration parameter settings are derived from the e*Way Connection.
 * This class can also parse this configuration information and provide it to you via
 * accessor methods.
 */
public class BatchRecordConfiguration {
    //private static final Messages mMessages =
    //        Messages.getMessages(BatchRecordConfiguration.class);
    private static final Logger mLogger =
            Messages.getLogger(BatchRecordConfiguration.class);
    
    private boolean bSynchronized;
    /**
     * Constructor; gets the properties object from the e*Way Connection.
     *
     * @param  props My properties. These properties come from the
     *         settings made in the e*Way Configuration Editor and
     *         contain the configuration parameters.
     */
    public BatchRecordConfiguration(Properties props) {
        //super(props);
        m_props = props;
        // The following are set in the parseProperties method
        // but I'll init them anyway.
        m_sParserClassName = null;
        m_userProps = null;
        m_nRecSize = 0;
        m_sRecType = null;
        m_byRecDelims = null;
        m_sRecDelims = null;
        m_bDelimOnLast = true;
        m_bCreateMode = false;
        parseProperties();
    }
    
    /**
     * Gets the record-parser class name. Which class depends on whether you
     * chose "Fixed", "Delimited", or "Single" in the e*Way Configuration Editor . If you
     * chose "User Class," the return is the class name you entered.
     */
    public String getParserClassName() { return m_sParserClassName; }
    
    
    /**
     * Gets the record type as it was set in the e*Way Configuration Editor. The method
     * returns "Fixed", "Delimited," or "Single."
     */
    public String getRecordType() { return m_sRecType; }
    
    
    /**
     * Gets the record delimiter as it was set in the e*Way Configuration Editor
     * but converted to a byte array. Only use this method if
     * you chose "Delimited" as the record type.
     */
    public byte[] getRecordDelimiter() { return m_byRecDelims; }
    
    
    /**
     * Gets the record delimiter as it was set in the e*Way Configuration Editor.
     * Only use this method if
     * you chose "Delimited" as the record type.
     */
    public String getRecordDelimiterAsString() { return m_sRecDelims; }
    
    
    /**
     * Gets the record size as it was set in the e*Way Configuration Editor.
     * Only use this method if you chose "Fixed" as the
     * record type.
     */
    public int getRecordSize() { return m_nRecSize; }
    
    
    /**
     * Gets the status of your setting in the e*Way Configuration Editor, which
     * indicates whether there is a delimiter on the last record in the
     * file. Only use this method if you chose "Delimited"
     * as the record type.
     */
    public boolean isDelimiterOnLast() { return m_bDelimOnLast; }
    
    
    /**
     * Returns whether create mode was selected in the e*Way Configuration Editor.
     * true if so, but false if otherwise (which indicates the parse mode).
     */
    public boolean isCreateMode() { return m_bCreateMode; }
    
    
    /**
     * Gets the user-defined properties and returns a null if there are no such properties.
     */
    public Properties getUserProperties() { return m_userProps; }
    
    
    // Internal helper to parse the properties
    protected void parseProperties() {
        if (null == m_props)
            return;
        
        // Will be set to true if user chose User Class
        boolean bIsUserClass = false;
        
        //
        // Determine record type and parser class name.
        // Note the delimiters and sizes and so forth are also parsed
        // in the parser. They are parsed here for two reasons.
        // 1. So we can display it in the trace for debugging and
        // 2. Because we also have to expose it on the ETD.
        // I don't care much for parsing the delimiters and sizes
        // and so forth twice but the purpose is different. Here
        // it is essentially for display purposes. In the parser
        // it is re-interpreted as a sequence of bytes.
        //
        String sTemp = m_props.getProperty("Record/Record Type");
        if (sTemp != null) {
            m_sRecType = sTemp;
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "BatchRecordConfiguration::parseProperties: record type = " + m_sRecType);
            }
            if (sTemp.equalsIgnoreCase("Delimited")) {
                m_sParserClassName = "com.sun.jbi.batchext.record.BatchDelimitedRecordParser";
                // Get the delimiter
                sTemp = m_props.getProperty("Record/Record Delimiter", "\n");
                m_sRecDelims = sTemp;
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "BatchRecordConfiguration::parseProperties: delimiters are " + sTemp);
                }
                m_byRecDelims = sTemp.getBytes();
                // Delimiter on last record in file?
                sTemp = m_props.getProperty("Record/Delimiter On Last Record", "Yes");
                if (sTemp.equalsIgnoreCase("Yes"))
                    m_bDelimOnLast = true;
                else
                    m_bDelimOnLast = false;
            } else if (sTemp.equalsIgnoreCase("Fixed")) {
                m_sParserClassName = "com.sun.jbi.batchext.record.BatchFixedRecordParser";
                // Get the record size
                Object obj = m_props.get("Record/Record Size");
                if ( obj != null )
                    sTemp = obj.toString();
                else
                    sTemp = "0";
                //sTemp = m_props.get("Record/Record Size");
                m_nRecSize = Integer.parseInt(sTemp);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "BatchRecordConfiguration::parseProperties: record size = " + m_nRecSize);
                }
            } else if (sTemp.equalsIgnoreCase("Single Record")) {
                m_sParserClassName = "com.sun.jbi.batchext.record.BatchSingleRecordParser";
            } else { // User Defined
                m_sParserClassName = m_props.getProperty("User Class/User Class", "");
                bIsUserClass = true;
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "BatchRecordConfiguration::parseProperties: user class name = " + m_sParserClassName);
                }
            }
        }
        
        //
        // If a user class, see if there's a user properties file to load
        //
        if (bIsUserClass) {
            sTemp = m_props.getProperty("User Class/User Properties", "");
            if ((sTemp != null) && (sTemp.length() > 1)) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "BatchRecordConfiguration::parseProperties: attempting to load the user properties file <" + sTemp + ">");
                }
                m_userProps = new Properties();
                try {
                    m_userProps.load(new FileInputStream(sTemp));
                } catch (IOException iox) {
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, "Exception thrown from properties.load of user properties: " + iox.getMessage());
                    }
                    m_userProps = null;
                }
            } else {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "BatchRecordConfiguration::parseProperties: no user property file detected");
                }
            }
        }
        
        //
        // Parse or Create Mode?
        //
        sTemp = m_props.getProperty("General Settings/Parse or Create Mode");
        if ((sTemp != null) && (sTemp.equalsIgnoreCase("Create"))) {
            m_bCreateMode = true;
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "BatchRecordConfiguration::parseProperties: create mode selected");
            }
        } else {
            m_bCreateMode = false;
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "BatchRecordConfiguration::parseProperties: parse mode selected");
            }
        }
        
        this.bSynchronized = m_props.getProperty("General Settings/Synchronized", "Yes").equalsIgnoreCase("Yes");
    }
    
    
    //
    // Member variables...
    //
    
    
    // These are the properties that were passed to us in the c'tor.
    private Properties m_props = null;
    
    // The class name of the parser. It depends on the settings in the
    // config. Set in parseProperties.
    private String m_sParserClassName = null;
    
    // If it is a user class and they provided a user properties file
    // name we'll load it in to here. Set in parseProperties.
    private Properties m_userProps = null;
    
    // This is one of the properties from the config. Need a holder for
    // it because we need to expose an accessor for it (it is exposed
    // on the ETD. Set in parseProperties.
    private int m_nRecSize = 0;
    
    // Record type from the config dialog (Fixed, Delimited, etc). Set
    // in parseProperties.
    private String m_sRecType = null;
    
    // Another property we need to expose for the ETD - the record
    // delimiters. Only meaningful in delimited record parsing. Set
    // in parseProperties.
    private byte[] m_byRecDelims = null;
    
    // This is the record delimiter string exactly as entered in the
    // config dialog. Mainly for internal use. Set in parseProperties.
    private String m_sRecDelims = null;
    
    // This is the config property that indicates if there is a record
    // delimiter on the last record in the file. Only meaningful if
    // they chose delimited. Set in parseProperties.
    private boolean m_bDelimOnLast = true;
    
    // This will be true if the user chose Create mode in the config.
    // Otherwise it'll be false (which indicates parse mode). It is
    // set in parseProperties.
    private boolean m_bCreateMode = false;
    
    // not exposed to collaboration code
    protected void setSynchronized(boolean b) {
        this.bSynchronized=b;
    }
    
    public boolean getSynchronized() {
        return this.bSynchronized;
    }
}
