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
 * @(#)BatchDelimitedRecordParser.java 
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

import java.io.InputStream;
import java.io.OutputStream;
import java.security.InvalidParameterException;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;


/**
 * This class is the implementation of the e*Way-supplied record parser for
 * the delimited record type.
 *
 * @see     BatchRecordParser
 */
public class BatchDelimitedRecordParser implements BatchRecordParser {
    //static final String RCS_ID = com.stc.Copyright.NOTICE+"";
    //private static final Messages mMessages =
    //        Messages.getMessages(BatchDelimitedRecordParser.class);
    private static final Logger mLogger =
            Messages.getLogger(BatchDelimitedRecordParser.class);
    
    public BatchDelimitedRecordParser() {
        m_bDelimOnLast = false;
        m_nDelimLen = 0;
        m_byDelims = null;
        m_bRecordsPut = false;
        m_bCreateMode = false;
    }
    
    
    public void initialize(BatchRecordConfiguration conf) throws Exception {
        // Only thing we need from the configuration are the delimiters
        // and whether there is a delimiter on the last record.
        // We will consider it an error if no delimiters are
        // found.
        if (null == conf)
            throw new Exception("Null BatchRecordConfiguration passed to BatchDelimitedRecordParser.configure");
        
        String sTemp = conf.getRecordDelimiterAsString();
        if (null == sTemp)
            throw new Exception("BatchDelimitedRecordParser.configure: no delimiters found in properties");
        
        m_byDelims = parseDelims(sTemp);
        if (null == m_byDelims)
            throw new Exception("BatchDelimitedRecordParser.configure: no delimiters found in properties");
        
        m_nDelimLen = m_byDelims.length;
        if (m_nDelimLen < 1) // overly cautious
            throw new Exception("BatchDelimitedRecordParser.configure: no delimiters found in properties");
        
        m_bDelimOnLast = conf.isDelimiterOnLast();
        
        m_bCreateMode = conf.isCreateMode();
    }
    
    
    public byte[] get(InputStream input) throws Exception {
        if (null == input) {
            throw new InvalidParameterException("BatchDelimitedRecordParser.get called with null InputStream.");
        }
        InputStreamFindAdapter isa = new InputStreamFindAdapter(input);
        
        isa.setDelimOnLastRecord(m_bDelimOnLast);
        
        // Perform error checks and exception handling
        // Require changes to InputStreamFindAdapter.find()
        
        InputStreamFindResult result = isa.find(m_byDelims);
        
        if (result.getRecordSize() < 0)
            return null;
        
        // Read the data
        byte[] baRet = new byte[result.getRecordSize()];
        isa.read(baRet, 0, result.getRecordSize());
        
        // Eat the delim
        if (result.isLastRecord() && !m_bDelimOnLast) {
            // do nothing if found the last record and the
            // last record does not end with a delimiter
        } else {  // eat the delim
            byte[] delim = new byte[m_nDelimLen];
            isa.read(delim, 0, m_nDelimLen);
        }
        
        return baRet;
    }
    
    
    public void put(OutputStream output, byte[] data) throws Exception {
        if (null == output) {
            throw new InvalidParameterException("BatchDelimitedRecordParser.put called with null OutputStream");
        }
        if (null == data) {
            throw new InvalidParameterException("BatchDelimitedRecordParser.put called with null data");
        }
        
        // Add the delimiters if a previous record was written
        if (m_bRecordsPut)
            output.write(m_byDelims, 0, m_nDelimLen);
        
        // Simple: Just append the data and add the delimiters.
        // The user should not have already added the delimiter.
        int nLenData = data.length;
        if (nLenData > 0) {
            // Add the new data
            output.write(data, 0, nLenData);
            m_bRecordsPut = true;
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "BatchDelimitedRecordParser.put: Successfully stored a record with size " + nLenData);
            }
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "BatchDelimitedRecordParser.put: Stored an empty record - the record is null or the size is zero");
            }
        }
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "BatchDelimitedRecordParser.put: Successfully stored the record delimiters");
        }
        
        // Flush
        output.flush();
        
        return;
    }
    
    public void finish(OutputStream output, InputStream input) throws Exception {
        // The only necessary check is in create mode, because
        // it might need to output the delimiter on last record.
        if (m_bCreateMode && (output == null)) {
            String msg = "BatchDelimitedRecordParser.finish: The OutputStream is null";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new Exception(msg);
        }
        
        // Add the delimiters
        if (m_bRecordsPut && m_bDelimOnLast)
            output.write(m_byDelims, 0, m_nDelimLen);
        
        m_bRecordsPut = false;
    }
    
    
    //
    // Internal helpers
    //
    
    
    // Exposed as public so I can include in the test suite
    // This routine takes a string representing delimiters separated
    // by a comma (if more than one). Supports octal, hex, single char
    // and escaped chars for NL, CR, TAB, and FF. See the
    // BatchRecord.def file for a wordy explanation.
    public byte[] parseDelims(String s) throws Exception {
        StringTokenizer tokenizer = new StringTokenizer(s, ",");
        int nTokens = tokenizer.countTokens();
        if (nTokens < 1)
            return null;
        
        String sTok, sTmp;
        int nTokLen, nb = 0;
        byte[] byToks = new byte[nTokens];
        
        do {
            sTok = tokenizer.nextToken();
            if (null == sTok)
                throw new Exception("null token returned from tokenizer");
            
            nTokLen = sTok.length();
            if (nTokLen < 1)
                throw new Exception("Zero length token returned from tokenizer");
            
            if (1 == nTokLen) {
                // Plain single byte. e.g. '*'
                byToks[nb] = sTok.getBytes()[0];
            } else if ('\\' == sTok.charAt(0)) {
                // An escaped sequence of some sort
                if (2 == nTokLen) {
                    // Single byte escaped. e.g. \r
                    if ('r' == sTok.charAt(1)) {
                        byToks[nb] = '\r';
                    } else if ('n' == sTok.charAt(1)) {
                        byToks[nb] = '\n';
                    } else if ('t' == sTok.charAt(1)) {
                        byToks[nb] = '\t';
                    } else if ('f' == sTok.charAt(1)) {
                        byToks[nb] = '\f';
                    } else
                        throw new Exception("Unsupported escape character");
                } else {
                    // Multi byte escaped. e.g. \0x0D
                    // decode may throw if illegal byte value
                    sTmp = sTok.substring(1);
                    try {
                        byToks[nb] = Byte.decode(sTmp).byteValue();
                    } catch (NumberFormatException ne) {
                        throw new Exception("Illegal byte value: " + sTmp);
                    }
                }
            } else {
                throw new Exception("Illegal multibyte sequence. Are all tokens separated by a comma?");
            }
            nb++;
        } while (tokenizer.hasMoreElements());
        
        return byToks;
    }
    
    
    //
    // Member variables...
    //
    
    private boolean m_bDelimOnLast = false;
    private int m_nDelimLen = 0;
    private byte[] m_byDelims = null;
    private boolean m_bRecordsPut = false;
    private boolean m_bCreateMode = false;
}
