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
 * @(#)BatchSingleRecordParser.java 
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

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.InvalidParameterException;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.batchext.streaming.StreamUtil;

/**
 * This class is the implementation of the e*Way-supplied record parser for
 * the Single record type.
 *
 * @see     BatchRecordParser
 */
public class BatchSingleRecordParser implements BatchRecordParser {
    //private static final Messages mMessages =
    //        Messages.getMessages(BatchSingleRecordParser.class);
    private static final Logger mLogger =
            Messages.getLogger(BatchSingleRecordParser.class);
    
    public BatchSingleRecordParser() {
    }
    
    public void initialize(BatchRecordConfiguration conf) throws Exception {
        recordProcessed = false;
    }
    
    public byte[] get(InputStream input) throws Exception {
        // The entire payload is the record.
        // A single call to get consumes the record.
        if (null == input) {
            throw new InvalidParameterException("BatchSingleRecordParser.get called with null InputStream.");
        }
        
        if (recordProcessed) {
            // No more records to read.
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "BatchSingleRecordParser.get: The record has been retrieved already.");
            }
            return null;
        }
        
        // Create the output stream with initial size equal to
        // the size of the currently available data
        ByteArrayOutputStream baos = new ByteArrayOutputStream(input.available());
        byte[] r = null;
        
        // Copy the input content to the temporary buffer
        StreamUtil.copyStream(input, baos, 65536);
        r = baos.toByteArray();
        recordProcessed = true;
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "BatchSingleRecordParser.get: Successfully retrieved a record with size " + r.length);
        }
        return r;
    }
    
    public void put(OutputStream output, byte[] data) throws Exception {
        if (null == output) {
            throw new InvalidParameterException("BatchSingleRecordParser.put called with null OutputStream");
        }
        if (null == data) {
            throw new InvalidParameterException("BatchSingleRecordParser.put called with null data");
        }
        if (recordProcessed) {
            throw new Exception("BatchSingleRecordParser.put: The record has been processed already.");
        }
        
        // Append the data to the payload.
        int nLenData = data.length;
        if (0 == nLenData) {
            // nothing to put
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "BatchSingleRecordParser.put: Nothing to store - the record is null or the size is zero");
            }
            return;
        }
        output.write(data, 0, nLenData);
        recordProcessed = true;
        output.flush();
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "BatchSingleRecordParser.put: Successfully stored a record with size " + nLenData);
        }
        return;
    }
    
    public void finish(OutputStream output, InputStream input) throws Exception {
        recordProcessed = false;
    }
    
    protected boolean recordProcessed = false;
}
