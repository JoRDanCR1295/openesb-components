/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.processor.transform;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.jbi.Messages;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Reader;
import java.io.Serializable;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;

import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamSource;

/**
 * Taken from servicemix-core.
 */
public class StringSource extends StreamSource implements Serializable {

	/** The Constant LOG. */
    private static final Logger LOG
    = LoggerFactory.getLogger(StringSource.class); 
    private static final Messages MESSAGES
    = Messages.getMessages(StringSource.class);
	
	/** The Constant serialVersionUID. */
    private static final long serialVersionUID = -2965901682811246960L;

    /** The text. */
    private final String text;
    
    /** The encoding. */
    private String encoding = "UTF-8";

    /**
     * Instantiates a new string source.
     * 
     * @param text
     *          The StringSource text
     */
    public StringSource(String text) {
        if (text == null) {
        	String msg=MESSAGES.getString("EJB000707_Text_can_not_be_null");
            LOG.error(msg);
            throw new NullPointerException(msg);   

        }
        this.text = text;
    }

    /**
     * Instantiates a new string source.
     * 
     * @param text
     *          The StringSource text
     * @param systemId
     *          The SystemId
     */
    public StringSource(String text, String systemId) {
        this(text);
        setSystemId(systemId);
    }

    /**
     * Instantiates a new string source.
     * 
     * @param text
     *          The StringSource text
     * @param systemId
     *          The SystemId
     * @param encoding
     *          The encoding
     */
    public StringSource(String text, String systemId, String encoding) {
        this.text = text;
        this.encoding=encoding;
        setSystemId(systemId);
    }

    /* (non-Javadoc)
     * @see javax.xml.transform.stream.StreamSource#getInputStream()
     */
    
    /**
     * Gets the <code>InptStream</code>.
     * 
     * @return
     *      The Source InputStream
     */   
    public InputStream getInputStream() {
        try {
            return new ByteArrayInputStream(text.getBytes(encoding));
        } catch (UnsupportedEncodingException e) {
        	String msg=MESSAGES.getString("EJB000708_Exception_in_getInputStream", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new RuntimeException(msg,e);
        }        
    }
       
    /**
     * Gets the Reader.
     * 
     * @return te source Reader
     */
    public Reader getReader() {
        return new StringReader(text);
    }
    
    /**
     * The Source as String.
     * 
     * @return the Source as String
     */
    public String toString() {
        return "StringSource[" + text + "]";
    }

    /**
     * Gets the text.
     * 
     * @return the text
     */
    public String getText() {
        return text;
    }

}
