/*
 * ChainBuilder ESB
 *          Visual Enterprise Integration
 *
 * Copyright (C) 2006 Bostech Corporation
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc.,59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 *
 *
 * $Id: ReaderToInputStream.java,v 1.1.1.1 2007/04/09 17:49:29 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;

/**
 *
 */
public class ReaderToInputStream extends InputStream {

	protected Reader reader;

	protected ByteArrayOutputStream byteArrayOut;

	protected Writer writer;

	protected char[] chars;

	protected byte[] buffer;

	protected int index, length;

	public ReaderToInputStream(Reader reader) {
		this.reader = reader;
		byteArrayOut = new ByteArrayOutputStream();
		writer = new OutputStreamWriter(byteArrayOut);
		chars = new char[1024];
	}

	public ReaderToInputStream(Reader reader, String encoding)
			throws UnsupportedEncodingException {
		this.reader = reader;
		byteArrayOut = new ByteArrayOutputStream();
		writer = new OutputStreamWriter(byteArrayOut, encoding);
		chars = new char[1024];
	}

	public int read() throws IOException {
		if (index >= length)
			fillBuffer();
		if (index >= length)
			return -1;
		return 0xff & buffer[index++];
	}

	protected void fillBuffer() throws IOException {
		if (length < 0)
			return;
		int numChars = reader.read(chars);
		if (numChars < 0) {
			length = -1;
		} else {
			byteArrayOut.reset();
			writer.write(chars, 0, numChars);
			writer.flush();
			buffer = byteArrayOut.toByteArray();
			length = buffer.length;
			index = 0;
		}
	}

	public int read(byte[] data, int off, int len) throws IOException {
		if (index >= length)
			fillBuffer();
		if (index >= length)
			return -1;
		int amount = Math.min(len, length - index);
		System.arraycopy(buffer, index, data, off, amount);
		index += amount;
		return amount;
	}

	public int available() throws IOException {
		return (index < length) ? length - index : ((length >= 0) && reader
				.ready()) ? 1 : 0;
	}

	public void close() throws IOException {
		reader.close();
	}
}
