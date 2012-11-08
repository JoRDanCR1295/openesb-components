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
 * $Id: ExternalInput.java,v 1.2 2007/04/16 11:41:29 tvolle Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;

import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.ByteArraySource;
import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.NormalizedMessageHandler;
import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.StringSource;

/**
 *  Read external input from a Reader or InputStream and transfer to normalized messages based on the
 *  read settings.
 */
public class ExternalInput {

	public static final Log log = LogFactory.getLog(ExternalInput.class);

	private final static String[] READ_STYLES = {"Read Style", "raw", "newline"};
    private final static int READ_STYLE_RAW = 0;
    private final static int READ_STYLE_NEWLINE = 1;

    private final static String[] REC_TYPES = {"Record Type", "xml", "string", "binary"};
    private final static int REC_TYPE_XML = 0;
    private final static int REC_TYPE_STRING = 1;
    private final static int REC_TYPE_BINARY = 2;
    
    private final static String IDENTITY_CHARSET = "ISO8859-1"; 
    
    private BufferedReader reader;
    private BufferedInputStream inputStream;
    private String charset;
    private int readStyle;
    private int recordType;
    private int recordsPerMessage;
    private boolean mayHaveMoreData = true;
    private String nextString;

    /*
     * Construct the external input source based on a Reader or an InputStream
     */
    public ExternalInput(Reader rdr, String cs, String rs, String rt, int rpm) throws Exception {
    	setupParameters(cs, rs, rt, rpm);
    	if (recordType != REC_TYPE_BINARY || readStyle == READ_STYLE_NEWLINE) {
    		// If we have a reader and want binary then set up a stream using the character set
    		reader = new BufferedReader(rdr);
    	} else inputStream = new BufferedInputStream(new ReaderToInputStream(rdr, charset));
    }
    public ExternalInput(InputStream instr, String cs, String rs, String rt, int rpm) throws Exception {
    	setupParameters(cs, rs, rt, rpm);
    	if (recordType == REC_TYPE_STRING || readStyle == READ_STYLE_NEWLINE) {
    		// If we want binary and newline then use an identity charset to avoid changing the
    		// data when converting to a reader and back.
    		if (recordType == REC_TYPE_BINARY) charset = IDENTITY_CHARSET;
    		// If we have an input stream but want characters then set up a reader
    		reader = new BufferedReader(new InputStreamReader(instr, charset));
    	} else inputStream = new BufferedInputStream(instr);
    }
    
    public boolean hasMoreData() {
    	boolean result = false;
    	if (mayHaveMoreData) {
    		if (readStyle == READ_STYLE_RAW) result = true;
    		else {
    			if (nextString != null) result = true;
    			else {
    				try {
    					nextString = reader.readLine();
    				}
    				catch (IOException e) {
    					log.warn("Error reading input: "+e+"\n"+ExceptionUtil.stackTraceString(e));
    				}
    				if (nextString != null) result = true;
    				else {
    					mayHaveMoreData = false;
    				}
    			}
    		}
    	}
    	return result;
    }
    
    /*
     * Use input data to populate a normalized message. Return the number of records added.
     */
    public int populateMessage(NormalizedMessage nm, QName messageQName) {
    	NormalizedMessageHandler nmh = new NormalizedMessageHandler(nm, messageQName);
    	if (readStyle == READ_STYLE_NEWLINE) populateNewline(nmh);
    	else populateRaw(nmh);
    	nmh.generateMessageContent();
     	return nmh.getRecordCount();
    }
    
    /*
     * newline style always has a reader and uses readLine. Read up to recordsPerMessage
     * new records.
     */
    private void populateNewline(NormalizedMessageHandler nmh) {
    	int newRecords=0;
		try {
			for (;;) {
				String thisString;
				Source src = null;
				if (nextString != null) {
					thisString = nextString;
					nextString = null;
				} else {
					 thisString = reader.readLine();
    				if (thisString == null) {
    					mayHaveMoreData = false;
    					break;
    				}
				}
				if (recordType == REC_TYPE_XML) {
					src = new StreamSource(new StringReader(thisString));
				} else if (recordType == REC_TYPE_BINARY) {
					src = new ByteArraySource(thisString.getBytes(charset));
				} else {
					src = new StringSource(thisString);
				}
				nmh.addRecord(src);
				if (recordsPerMessage > 0 && (++newRecords == recordsPerMessage)) break;
			}
		}
		catch (IOException e) {
			ioErrorWarn(e);
		}
    }
    
    /*
     * raw style reads the entire stream at once
     */
    private void populateRaw(NormalizedMessageHandler nmh) {
    	mayHaveMoreData = false;
    	Source src=null;
    	if (recordType == REC_TYPE_XML) {
    		if (reader != null) src = new StreamSource(reader);
    		else src = new StreamSource(inputStream);
    	} else if (recordType == REC_TYPE_BINARY) {
    		ByteArrayOutputStream baos = new ByteArrayOutputStream();
    		byte[] buffer = new byte[2048];
    		int len;
			try {
	    		while ((len=inputStream.read(buffer)) > 0) baos.write(buffer, 0, len);
			}
			catch (IOException e) {
				ioErrorWarn(e);
			}
    		src = new ByteArraySource(baos.toByteArray());
    	} else {
    		StringWriter sw = new StringWriter();
    		char[] buffer = new char[2048];
    		int len;
			try {
	    		while ((len=reader.read(buffer)) > 0) sw.write(buffer, 0, len);
			}
			catch (IOException e) {
				ioErrorWarn(e);
			}
    		src = new StringSource(sw.toString());
    	}
    	nmh.addRecord(src);
    }
    

    private void setupParameters(String cs, String rs, String rt, int rpm) throws Exception {
    	charset = cleanupCharset(cs);
    	readStyle = findInArray(rs, READ_STYLES);
    	recordType = findInArray(rt, REC_TYPES);
    	recordsPerMessage = rpm;
    }
    
    private static final String defaultCharset = (new InputStreamReader(new ByteArrayInputStream(new byte[0]))).getEncoding();

    private String cleanupCharset(String cs) {
    	String result = null;
    	if (cs == null || cs.length()<1) result = defaultCharset;
    	else result = cs;
    	return result;
    }
    
    private int findInArray(String s, String arr[]) throws Exception {
    	for (int i=1; i<arr.length; i++) if (s.equals(arr[i])) return i-1;
    	throw new Exception("invalid "+arr[0]+" \""+s+"\"");
    }
    
    private void ioErrorWarn(Exception e) {
		log.warn("Error reading external input: "+e+"\n"+ExceptionUtil.stackTraceString(e));
    }
}