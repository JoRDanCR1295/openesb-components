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
 * @(#)EmailAttachment.java 
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

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * This class implements the email attachment.
 * 
 * @author  
 * @version $Version$
 */
public class EmailAttachment {
    private byte [] content = null;
    private String  contentType = null;
    private String  name = null;
    private ByteArrayOutputStream bos = null;
    
    /** 
     * Creates new EmailAttachment.
     */
    public EmailAttachment() {
    }
    
    /** 
     * Sets the attachment content.
     * 
     * @param   content     The attachment content as a byte array.
     */
    public void setContent (final byte[] content) {
        this.content = content;
    }

    /** 
     * Gets the attachment content.
     * 
     * @return  The attachment content as a byte array.
     */
    public byte [] getContent () {
        if (content == null) {
            if ((bos != null) && (bos.size() > 0)) {
                content = bos.toByteArray();
            } 
            // QAI 87629
            else {
               final ByteArrayOutputStream new_bos = new ByteArrayOutputStream();
               content = new_bos.toByteArray();
            }
        }        
        return content;
    }
    
    /** 
     * Sets the attachment content type (MIME).
     * 
     * @param   contentType     The attachment content type (MIME).
     */
    public void setContentType (final String contentType) {
        this.contentType = contentType;
    }

    /** 
     * Gets the attachment content type (MIME).
     * 
     * @return  The attachment content type (MIME).
     */
    public String getContentType () {
        return retrieveContentType();
    }

    /** 
     * Sets the attachment name (filename).
     * 
     * @param   name     The attachment name (filename).
     */
    public void setName (final String name) {
        this.name = name;
		  if(this.name.length() == 1)
		  {
			  this.name = this.name + " ";
		  }
    }

    /** 
     * Gets the attachment name (filename).
     * 
     * @return  The attachment name (filename).
     */
    public String getName () {
        return name;
    }
    
    /** 
     * Set the content type as a ByteArrayOutputStream.
     * 
     * @param   bos     The ByteArrayOutputStream attachment content.
     */
    protected void setContent (final ByteArrayOutputStream bos) {
        this.bos = bos;
    }

    /**
     * Reads the attachment file from the InputStream <code>is</code>
     * and stores its content into internal buffer provided by the
     * ByteArrayOutputStream <code>bas</code>.
     * 
     * @param   is  The InputStream.
     * 
     * @return  The contents read from the InputStream as a byte array or
     *          null if nothing was read from the InputStream.
     * 
     * @throws  IOException upon error.
     */
    protected void readFrom(final InputStream is) throws IOException {
        final BufferedInputStream bis = new BufferedInputStream(is);
        final ByteArrayOutputStream tbos = new ByteArrayOutputStream();
        final byte [] buffer = new byte [1024];
        while (true) {
            final int bytesRead = bis.read (buffer);
            if (bytesRead == -1) {
                break;
            }
            tbos.write (buffer, 0, bytesRead);
        }
        bis.close();
        tbos.close();
        
        if (tbos.size() > 0) {
            content = tbos.toByteArray();
        }
    }
        
    /** 
     * Get the content type based on the attributes set.
     * 
     */
    protected String retrieveContentType () {
        String temp = contentType;
        if ( (temp == null) || (temp.length() == 0) ) {
            if (name != null) {
                temp = AttachmentDataSource.fileTypeMap.getContentType(name);
            } else {
                temp = "application/octet-stream";
            }
        }
        return temp;
    }
    
    /** 
     * Return the information of this EmailAttachment.
     * 
     * @return  The String form of this EmailAttachment.
     * 
     */
    @Override
	public String toString() {
        return new String ("ContentType [" + contentType +
                           "], Name [" + name + 
                           "], ContentLength [" + 
                           (content==null? "no payload":Integer.toString(content.length)) + "]");
    }
    
}
