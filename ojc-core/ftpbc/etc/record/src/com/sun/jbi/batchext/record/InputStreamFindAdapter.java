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
 * @(#)InputStreamFindAdapter.java 
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

/**
 * This class is a shim to the java.io.InputStream interface.  Constructor
 * takes the InputStream and then this class provides similar methods as the
 * InputStream plus a few more to make dealing with the stream in the record
 * parser a bit easier. Where practical, the method names from the InputStream
 * interface are used and, if so, the semantics are the same as the InputStream
 * interface.
 *
 * Important: To provide for the ability to "peek", the InputStream that is
 * passed to the constructor must support skipping backwards. The
 * java.io.FileInputStream does. The ByteArrayInputStream does not. To use
 * a ByteArrayInputStream, see the BatchByteArrayInputStream.
 *
 * @author  jim.fu@sun.com
 * @version 
 * @see     BatchRecordParser
 * @see     BatchByteArrayInputStream
 * @see     java.io.InputStream
 */
public class InputStreamFindAdapter {
    /**
     * Constructor - pass the InputStream we are to wrap. Must not
     * be null.
     */
    public InputStreamFindAdapter(InputStream is) throws Exception {
        if (null == is)
            throw new Exception("Null InputStream passed to InputStreamFindAdapter constructor");
        m_is = is;
    }


    /**
     * Get the total number of bytes that can be read. Routes to the
     * similarly named method in the InputStream. As you read more
     * and more of the stream, this method will return a lower and
     * lower value.
     *
     * @throws Exception If there is a problem determining this
     *         or if underlying InputStream throws an exception.
     */
    public int available() throws Exception {
        return m_is.available();
    }


    /**
     * Read 'count' bytes into 'buffer' starting at current position
     * in the InputStream.
     *
     * @param buffer The caller allocated buffer to fill with bytes
     * @param count The number of bytes to put in buffer
     * @return The number of bytes placed in buffer.
     *
     * @throws Exception If there is a problem getting the bytes.
     *         It is an error if there are not 'count' bytes to
     *         get from current position (i.e. try to get more
     *         than what is available).
     */
    public int read(byte[] buffer, int count) throws Exception {
        return read(buffer, 0, count);
    }


    /**
     * Semantically equiv to the same read method in the InputStream.
     */
    public int read(byte[] buffer, int off, int count) throws Exception {
        int n = m_is.read(buffer, off, count);
        return n;
    }


    /**
     * This function peeks ahead and searches for 'b' starting at the
     * current position in the stream. If found, returns the zero based
     * index as an offset from the current position, otherwise -1.
     * 
     * @param b The byte to look for.
     *
       * @returns InputStreamFindResult which represents the results of
       *          the attempt to find the next delimited record.
       *
     * @throws Exception If there's a problem navigating the stream.
     */
    public InputStreamFindResult find(byte b) throws Exception {
        byte[] ba = new byte[1];
        ba[0] = b;
        InputStreamFindResult n = find(ba);
        ba = null;
        return n;
    }

    /**
     * Sets the property which determines how the find method will interpret 
         * the delimiter for the last record.  If set to true, then the last record 
         * must end with a delimiter.  If set to false, the last record does not 
         * have to end with a delimiter.  The default value for this property is
         * true (the last record must end with the delimiter).
     * 
         * @param   bDelimOnLast    Set to true if delimiter is required for the last
         *                          record; otherwise, set to false.
         *
         * @see     #getDelimOnLastRecord
         * @see     #find
     *
     */
        public void setDelimOnLastRecord (boolean bDelimOnLast)
        {
            this.m_bDelimOnLast = bDelimOnLast;
        }
        
    /**
     * Gets the property which determines how the find method will interpret 
         * the delimiter for the last record.  
         *
         * @returns     If find looks for the delimiter following the last record,
                        then true will be returned; otherwise if find does not look
         *              for the delimiter following the last record, then false will
         *              be returned.
         *
         * @see     #setDelimOnLastRecord
         * @see     #find
     *
     */
        public boolean getDelimOnLastRecord ()
        {
            return m_bDelimOnLast;
        }

    /**
     * This function peeks ahead and searches for 'arr' starting at the
     * current position in the stream. If found, returns the zero based
     * index as an offset from the current position, otherwise -1.
     * 
     * @param arr The byte array to look for.
     *
         * @returns InputStreamFindResult which represents the results of
         *          the attempt to find the next delimited record.
         *
     * @throws Exception If there's a problem navigating the stream.
     */
    public InputStreamFindResult find(byte[] arr) throws Exception {
                boolean isLastRecord = false;
        int nLenArr = (null == arr) ? 0 : arr.length;
        if (nLenArr < 1)
            return new InputStreamFindResult (-1, false);
                
        int i, nRet = -1, nBytesRead = 0, nBytesDelimCheck = 0, intRead;
                
                while ( (intRead = m_is.read()) != -1)
                {
                    nBytesRead++;
                    if ( ((byte)intRead) == arr[0])   // found first byte of delimiter
                    {
                        nBytesDelimCheck++;
                        for (i=1; i < nLenArr; i++)
                        {
                            intRead = m_is.read();
                            nBytesRead++;
                            if (intRead == -1 || ( ((byte)intRead) != arr[i]))
                                break;
                            nBytesDelimCheck++;
                        }
                    }
                                                         
                    // If the count of the bytes in the inner loop matched
                    // the length of the delimiter, then we found the delimiter.
                    // The position to return is one less the number of bytes read
                    // before the start of the delimiter. Else -1.
                    nRet = (nBytesDelimCheck == nLenArr) ? nBytesRead-nLenArr : -1;
                    
                    // Break if found record or if EOF reached before record was found.
                    if ((nBytesDelimCheck == nLenArr) || intRead == -1 ) {
                        break;
                    }
                    else {
                        nBytesDelimCheck = 0;
                    }
                }
                
                // If end of file and didn't find a delimiter ...
                //    1. return the number of bytes read if the delimiter does NOT
                //       have to follow the last record.
                //    2. throw an exception if delimiter must follow the last record.
                if ( (intRead == -1) && (nBytesDelimCheck != nLenArr))
                {
                    if (nBytesRead != 0)    // Read some byte(s), otherwise return -1
                    {
                        if (!m_bDelimOnLast)
                        {
                            nRet = nBytesRead;
                            isLastRecord = true;
                        }
                        else
                            throw new Exception ("The last record does not end with a delimiter but the Delimiter On Last Record property was set to true.");
                    }
                }
                
                boolean bSkipOneMoreByte = false;
                
                // If found a delimiter ...
                //    throw an exception if the delimiter should not follow the last record.
                if ( (nBytesDelimCheck == nLenArr) )
                    if ( m_is.read() == -1 )
                    {
                        if (!m_bDelimOnLast)
                        {
                            throw new Exception ("The last record ended with a delimiter but the Delimiter On Last Record property was set to false.");
                        }
                        isLastRecord = true;
                    }
                    else
                    {
                        bSkipOneMoreByte = true;
                    }
                
        // Reset stream cursor to where we started
                nBytesRead = nBytesRead + (bSkipOneMoreByte?1:0);
        m_is.skip((int)0 - nBytesRead);
                
                return new InputStreamFindResult (nRet, isLastRecord);
    }


    /*
     * Member variables...
     */


    /**
     * The input stream passed to the constructor
     */
    protected InputStream m_is = null;
        protected boolean     m_bDelimOnLast = true;
}
