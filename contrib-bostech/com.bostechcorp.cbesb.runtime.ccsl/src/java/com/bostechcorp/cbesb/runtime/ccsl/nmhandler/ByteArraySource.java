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
 * $Id: ByteArraySource.java,v 1.1.1.1 2007/04/09 17:49:29 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.nmhandler;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;

import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

/**
 * A helper class which provides a JAXP {@link Source} from a byte[]
 * which can be read as many times as required.
 *
 */
public class ByteArraySource extends StreamSource {

	protected byte[] bytes;
	protected Charset charset;
	
	public ByteArraySource(byte[] bytes)
	{
		this.bytes = bytes;
		charset = Charset.defaultCharset();
	}
	
	public ByteArraySource(byte[] bytes, String systemId)
	{
		this.bytes = bytes;
		charset = Charset.defaultCharset();
		setSystemId(systemId);
	}

    public InputStream getInputStream() {
        return new ByteArrayInputStream(bytes);
    }

    public Reader getReader() {
        return new InputStreamReader(getInputStream(), charset);
    }
    
    public String toString() {
    	try {
    		return "ByteArraySource[" + new String(bytes, charset.displayName()) + "]";
    	}
    	catch(UnsupportedEncodingException e)
    	{
    		return "StringSource[UNABLE TO DISPLAY]";
    	}
    }

	public Charset getCharset() {
		return charset;
	}

	public void setCharset(Charset charset) {
		this.charset = charset;
	}

	public byte[] getBytes() {
		return bytes;
	}

}
