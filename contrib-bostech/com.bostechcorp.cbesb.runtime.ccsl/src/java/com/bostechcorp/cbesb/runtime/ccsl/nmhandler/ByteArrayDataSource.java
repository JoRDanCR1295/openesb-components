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
 * $Id: ByteArrayDataSource.java,v 1.1.1.1 2007/04/09 17:49:29 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.nmhandler;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.activation.DataSource;

/**
 * A helper class which provides a {@link DataSource} from a byte[]
 * which can be read as many times as required.
 */
public class ByteArrayDataSource implements DataSource {

    private byte[] data;
    private String contentType;
    private String name = "unused";

    public ByteArrayDataSource(byte[] data)
    {
    	this(data, null, null);
    }
    
    public ByteArrayDataSource(byte[] data, String contentType)
    {
    	this(data, contentType, null);
    }
    
    public ByteArrayDataSource(byte[] data, String contentType, String name)
    {
    	this.data = data;
    	this.contentType = contentType;
    	this.name = name;
    }
    
	public String getContentType() 
	{
		return contentType;
	}

	public InputStream getInputStream() throws IOException 
	{
		if (data == null)
		{
			throw new IOException("no data.");
		}
		return new ByteArrayInputStream(data);
	}

	public String getName() 
	{
		return name;
	}

	public OutputStream getOutputStream() throws IOException 
	{
		throw new IOException("getOutputStream() not supported.");
	}

	public byte[] getData()
	{
		return data;
	}
}
