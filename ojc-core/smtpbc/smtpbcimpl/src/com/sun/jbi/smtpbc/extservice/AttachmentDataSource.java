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
 * @(#)AttachmentDataSource.java 
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.logging.Logger;
import javax.activation.DataSource;
import javax.activation.FileTypeMap;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.smtpbc.SMTPDenormalizer;

/**
 * This class implements the DataSource to provide an in memory management
 * of attachments.
 * 
 * The DataSource interface provides the JavaBeans Activation Framework 
 * with an abstraction of some arbitrary collection of data. It provides 
 * a type for that data as well as access to it in the form of InputStreams
 * and OutputStreams where appropriate. 
 * 
 * @author  
 * @version $Version$
 */
public class AttachmentDataSource implements DataSource {

    /**
     * The FileTypeMap
     */
    protected static FileTypeMap fileTypeMap = FileTypeMap.getDefaultFileTypeMap();
    
    private final EmailAttachment attachment;
   
    private static final Logger mLogger = Messages.getLogger(AttachmentDataSource.class);
    
    /**
     * Creates a new AttachmentDataSource.
     */
    public AttachmentDataSource (EmailAttachment attachment) {
        this.attachment = attachment;
        mLogger.fine ("Constructor; EmailAttachment " +
                       this.attachment.toString());
    }
    
	/**
	 * This method returns an InputStream representing the the data 
     * and throws the appropriate exception if it can not do so. 
     * Note that a new InputStream object must be returned each time 
     * this method is called, and the stream must be positioned at the
     * beginning of the data.
     * 
     * @return  An InputStream.
     * 
     * @throws  IOException upon error.
	 */
	public InputStream getInputStream() throws IOException {
        mLogger.fine ("getInputStream called on EmailAttachment " +
                       attachment.toString());
        return new ByteArrayInputStream (attachment.getContent());
	}
    
    /**
     * This method returns an OutputStream where the data can be written 
     * and throws the appropriate exception if it can not do so. Note 
     * that a new OutputStream object must be returned each time this 
     * method is called, and the stream must be positioned at the location 
     * the data is to be written.
     * 
     * @return  An OutputStream.
     * 
     * @throws  IOException upon error.
     */
    public OutputStream getOutputStream() throws IOException {
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        attachment.setContent(bos);
        mLogger.fine ("getOutputStream called");
        return bos;
    }
    
    /**
     * This method returns the MIME type of the data in the form of a 
     * string. It should always return a valid type. It is suggested that 
     * getContentType return "application/octet-stream" if the DataSource 
     * implementation can not determine the data type.
     * 
     * @return  The MIME type.
     */
    public String getContentType() {
        mLogger.fine ("getContentType called on EmailAttachment " +
                       attachment.toString());
       return attachment.retrieveContentType();
    }
    
    /**
     * Return the name of this object where the name of the object is 
     * dependant on the nature of the underlying objects. DataSources 
     * encapsulating files may choose to return the filename of the object.
     * (Typically this would be the last component of the filename, not 
     * an entire pathname.)
     * 
     * @return  The name of the object.
     */
    public java.lang.String getName() {
        mLogger.fine ("getName called on EmailAttachment " +
                       attachment.toString());
        return attachment.getName();
    }
}
