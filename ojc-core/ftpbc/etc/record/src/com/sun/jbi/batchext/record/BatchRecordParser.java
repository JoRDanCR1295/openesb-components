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
 * @(#)BatchRecordParser.java 
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


/**
 * The BatchRecordParser interface defines the interface used by all record parsers in the e*Way. If you 
 * want to provide your own parser, you must override each method defined in this interface and adhere to 
 * the signature and semantics of these methods. The BatchRecordParser interface is used for either 
 * parsing a payload or for creating a payload made up of records. The e*Way supplies parsers 
 * that know how to parse or create payloads of delimited records, fixed-sized records, and single records.
 *
 * A single instance of a record-processing parser in a Collaboration is not designed to be used for parsing 
 * and creating at the same time. That is, each instance is used for parsing a payload only or for creating 
 * a payload only. This usage dictates that two interfaces must be defined; one for parsing and one for 
 * creating.
 *
 * The parse/create features of the e*Way operate in this way allowing you to easily implement your own 
 * parser, if desired. Also, this operation makes setting the configuration parameters of the e*Way easier. 
 * You are configuring a parsing ETD or a creating ETD and not having to configure the 
 * complexities of an ETD that does both at the same time.
 *
 * Set up payload parsing as follows:
 *
 * 1.   Configure the parser in the e*Way Configuration Editor. Be sure to specify the record type.
 * 2.   Create a Collaboration that contains the parser.
 * 3.   Call setPayload() on the source data to parse it.
 * 4.   Call get() multiple times to extract the records from the data payload.
 *
 * Set the source data for parsing using either:
 *  o  setPayload()
 *  o  setInputStreamAdapter()
 *
 * These methods are mutually exclusive, that is, you can call one or the other but not both. The 
 * setPayload() method is used when the source data is already in memory as a byte array, for example, 
 * when the source data is of a relatively small size. You must use the setInputStreamAdapter() method 
 * when the source data is 
 * relatively large, and you do not want to load the entire payload into e*Gate's memory. This method 
 * employs the e*Way's data-streaming feature.
 *
 * In this case, the source can be any object that implements the 
 * com.sun.jbi.batchext.streaming.InputStreamAdapter interface, as long as the InputStream 
 * provided by that implementation supports the ability to seek backward, as the FileInputStream does. 
 * The ByteArrayInputStream does not. To use a ByteArrayInputStream, use the extension class 
 * BatchByteArrayInputStream instead.
 *
 * Create a payload as follows:
 *
 * 1.   Configure the parser in the e*Way Configuration Editor. Be sure to specify the record type.
 * 2.   Create a Collaboration that contains the parser.
 * 3.   Call put() multiple times to add records to the data payload.
 * 4.   Call getPayload() to get the buffer containing all the records.
 *
 * The setOutputStreamAdapter() method can be called to specify the stream- based destination of the 
 * data payload. This method also employs data streaming. The object instance retrieved can be any 
 * object that implements the com.sun.jbi.batchext.streaming.OutputStreamAdapter interface. 
 * This method is normally used if the payload created is of substantial size, and you do not want to create 
 * the data payload in e*Gate's memory. If this method is not called, the payload is created in an internal 
 * buffer, and you must call the getPayload() method to retrieve it, once all records have been added.
 *
 * Each method declares that an exception of the base javaException can be thrown. If an exception is 
 * thrown, it is caught by the ETD implementation code and sent to the e*Gate system to be 
 * logged. It is recommended that you provide a meaningful error message for the text of any of 
 * your exceptions. In general, the e*Way-supplied implementations do not throw these exceptions.
 * In cases where they do, it is noted.
 *
 * NOTE: The code that creates an instance of your object looks for a constructor that takes void. Your 
 * object must, therefore, provide a default, public constructor taking void, that is, a constructor 
 * with no parameters.
 */

public interface BatchRecordParser {
        
    /**
     * This method is called by the underlying implementation 
     * immediately after the instance of the class is created. Aside 
     * from the constructor, it is guaranteed to be the first method 
     * called on this object. This method allows to configure your own 
     * internals.
     *
     * @param conf   An instance of the BatchRecordConfiguration class, which 
     *               contains configuration-specific information for 
     *               the parser.
     *
     * @throws Exception If there is a problem with the 
     *         properties passed in. The e*Way-supplied parsers can 
     *         throw an exception if the configuration properties 
     *         are incorrect.
     */
    public void initialize(BatchRecordConfiguration conf)
        throws Exception;


    /**
     * The get() method in the ETD calls this method to do the desired task.
     * The semantics of the method mean, "Get the next record whatever that might 
     * be." For example, for fixed-sized records, this method returns the 
     * next record in the input of that size. This usage means that you 
     * have already dragged and dropped the data payload onto 
     * the Payload node in the ETD or set up a data-streaming 
     * link with another ETD.
     *
     * Also, the semantics of this method mean that 
     * it can, and usually is, called multiple 
     * times to extract records one by one from the data payload. 
     * No destructive actions are performed on the input data.
     *
     * @param input Specifies the input to get the 
     *          next record from.
     *
     * @throws Exception If there is an error in parsing the input.
     *
     * @return Returns the extracted record or a null when the input 
     *          has been "consumed." A consumed input means that get has been called 
     *          multiple times, and there are no more records left to get. 
     *          The e*Way-supplied parsers do not throw an exception in 
     *          this case.
     */
    public byte[] get(InputStream input) throws Exception;


    /**
     * The put() method in the ETD calls this method to do the desired task.
     * The semantics of this method mean, "Here is another record to be appended 
     * to the output file". For example, for fixed-sized records, the payload data 
     * is assumed to be a blob of data of that size, and that 
     * blob must be appended to the payload buffer.
     *
     * Also, the semantics of this method mean that it is the parser's 
     * responsibility to inspect the data being passed in, for validity.
     *
     * Ultimately, after you have called this method and built 
     * an output file, you then generally use 
     * the Payload ETD node to access that output. Alternatively, you can direct 
     * that output to another ETD using data streaming.
     *
     * @param output Specifies the output file or ETD (for data streaming) where a given 
     *          record goes.
     *
     * @param data The data to be included in 
     *          the record.
     *
     * @throws Exception If there is a problem processing the data. 
     *          The e*Way-supplied parsers throw exceptions 
     *          from this method if there is a problem with 
     *          the data, for example, for the fixed-size 
     *          record parser if the data is not the correct 
     *          size.
     */
    public void put(OutputStream output, byte[] data) throws Exception;
    
    /**
     * You must call this method when you are done with a transfer, to clean up 
     * resources, finish processing, and so on. Once this method is called, the parser 
     * must not be used again.
     *
     * @param   output  Specifies the output where the 
     *                  records go.
     * @param   input   Specifies the input where the 
     *                  records come from.
     *
     * @throws  Exception   If there is a problem finishing.
     */
    public void finish(OutputStream output, InputStream input) throws Exception;
    
}
