 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.processor.transform;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Reader;
import java.io.Serializable;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;

import javax.xml.transform.stream.StreamSource;

/**
 * Taken from servicemix-core.
 */
public class StringSource extends StreamSource implements Serializable {

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
            throw new NullPointerException("text can not be null");
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
            throw new RuntimeException(e);
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
