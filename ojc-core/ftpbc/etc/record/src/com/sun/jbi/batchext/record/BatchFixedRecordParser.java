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
 * @(#)BatchFixedRecordParser.java 
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
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;


/**
 * This class is the implementation of the e*Way-supplied record parser for
 * the fixed record type.
 *
 * @see     BatchRecordParser
 */
public class BatchFixedRecordParser implements BatchRecordParser {
    //private static final Messages mMessages =
    //        Messages.getMessages(BatchFixedRecordParser.class);
    private static final Logger mLogger =
            Messages.getLogger(BatchFixedRecordParser.class);
    
    public BatchFixedRecordParser() {
        m_nRecSize = 0;
    }
    
    
    public void initialize(BatchRecordConfiguration conf) throws Exception {
        // Only thing we need from the props is the record size
        // We will consider it an error if the size is negative
        // zero, or not found.
        if (null == conf)
            throw new Exception("Null BatchRecordConfiguration passed to BatchFixedRecordParser.configure");
        
        m_nRecSize = conf.getRecordSize();
        if (m_nRecSize < 1)
            throw new Exception("BatchFixedRecordParser.configure: invalid record size (must be greater than zero)");
    }
    
    
    public byte[] get(InputStream input) throws Exception {
        if (null == input) {
            throw new InvalidParameterException("BatchFixedRecordParser.get called with null InputStream.");
        }
        
        // Read another record and return it
        byte[] r = new byte[m_nRecSize];
        if (null == r) {
            throw new Exception("BatchFixedRecordParser.get: can not allocate memory for the record buffer.");
        }
        int nRead = input.read(r, 0, m_nRecSize);
        if (-1 == nRead) {
            // no data available
            return null;
        } else if (nRead != m_nRecSize) {
            throw new Exception("BatchFixedRecordParser.get: The record size does not match - expected "
                    + m_nRecSize + ", read " + nRead);
        }
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "BatchFixedRecordParser.get: Successfully retrieved a record with size " + m_nRecSize);
        }
        return r;
    }
    
    
    public void put(OutputStream output, byte[] data) throws Exception {
        if (null == output) {
            throw new InvalidParameterException("BatchFixedRecordParser.put called with null OutputStream");
        }
        if (null == data) {
            throw new InvalidParameterException("BatchFixedRecordParser.put called with null data");
        }
        
        int nLenData = data.length;
        if (0 == nLenData) {
            // nothing to put
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "BatchFixedRecordParser.put: Nothing to store - the record is null or the size is zero");
            }
            return;
        } else if (nLenData != m_nRecSize) {
            // invalid record
            throw new InvalidParameterException("BatchFixedRecordParser.put: The record size does not match - size is "
                    + nLenData + ", expected " + m_nRecSize);
        }
        
        output.write(data, 0, nLenData);
        output.flush();
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "BatchFixedRecordParser.put: Successfully stored a record with size " + nLenData);
        }
        return;
    }
    
    public void finish(OutputStream output, InputStream input) throws Exception {
    }
    
    // Member variables...
    private int m_nRecSize = 0;
}
