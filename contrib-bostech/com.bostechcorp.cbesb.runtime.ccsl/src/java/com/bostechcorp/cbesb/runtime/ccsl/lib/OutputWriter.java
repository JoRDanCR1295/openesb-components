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
 * $Id: OutputWriter.java,v 1.1.1.1 2007/04/09 17:49:29 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.io.ByteArrayInputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;

import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;

import com.bostechcorp.cbesb.common.util.Dom;
import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.ByteArraySource;
import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.StringSource;

/**
 * @author j.zhang
 * @version 1.0.0
 */
public class OutputWriter {

    /**
     * Write the StringSource to OutputStream.
     *
     * @param src The StringSource.
     * @param os The OutputStream.
     * @throws Exception
     */
    public static void writeStringSource(StringSource src, OutputStream os, String charset) throws Exception {
        os.write(src.getText().getBytes(charset));
    }

    /**
     * Write the ByteArraySource to OutputStream.
     *
     * @param src The ByteArraySource.
     * @param os The OutputStream.
     * @throws Exception
     */
    public static void writeByteArraySource(ByteArraySource src, OutputStream os) throws Exception {
        os.write(src.getBytes());
    }

    /**
     * Process the src to the OutputStream.
     *
     * @param src The Source object.
     * @param os OutputStream.
     * @param writeStyle The value of WriteStyle.
     * @param charset The value of Charset.
     */
    public static void processOutputStream(Source src, OutputStream os, String writeStyle, String charset) {
        try {
        	if (charset == null || charset.length()<1) {
        		charset = (new InputStreamReader(new ByteArrayInputStream(new byte[0]))).getEncoding();
        	}
            if (src instanceof DOMSource) {
                Dom.writeDOMSource((DOMSource)src, os, charset);
            } else if (src instanceof StringSource) {
                writeStringSource((StringSource)src, os, charset);
            } else if (src instanceof ByteArraySource) {
                writeByteArraySource((ByteArraySource)src, os);
            }
        } catch (Exception e) {
            System.err.println("error process src to OutputStream " + e);
            e.printStackTrace();
        }
    }
}
