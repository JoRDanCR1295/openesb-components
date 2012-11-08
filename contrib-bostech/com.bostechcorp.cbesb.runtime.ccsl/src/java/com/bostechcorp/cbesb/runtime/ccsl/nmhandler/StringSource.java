/*
 * ChainBuilder ESB
 *          Visual Enterprise Integration
 * 
 * Copyright (C) 2006 Bostech Corporation
 * 
 * This program is free software; you can redistribute it and/or modify it 
 * under the terms of the GNU General Public License as published by the 
 * Free Software Foundation; either version 2 of the License, or (at your option) 
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License 
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License along with 
 * this program; if not, write to the Free Software Foundation, Inc., 
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *
 * $Id: StringSource.java,v 1.1.1.1 2007/04/09 17:49:30 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.nmhandler;

import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

/**
 * A helper class which provides a JAXP {@link Source} from a String
 * which can be read as many times as required.

 */
public class StringSource extends StreamSource {
    private String text;

    public StringSource(String text) {
        this.text = text;
    }

    public StringSource(String text, String systemId) {
        this.text = text;
        setSystemId(systemId);
    }

    public InputStream getInputStream() {
    	ByteArrayInputStream bais = null;
    	try {
    		bais = new ByteArrayInputStream(text.getBytes("utf-8"));
    	} catch (Exception e) {
    		System.err.println("Invalid Encoding Exception: "+e);
    		e.printStackTrace();
    	}
    	return bais;
    }

    public Reader getReader() {
        return new StringReader(text);
    }
    
    public String toString() {
    	return "StringSource[" + text + "]";
    }

    public String getText() {
        return text;
    }

}
