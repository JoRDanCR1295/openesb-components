 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.cxf;
        
	import java.io.IOException;
	import java.io.InputStreamReader;
	import java.io.LineNumberReader;
	import java.net.URL;

	/**
	 * Provides information about a file that
	 */
	public class Jbi4CorbaIncludeStackEntry {
	    private final URL url;

	    private LineNumberReader reader;

	    private final String location;

	    Jbi4CorbaIncludeStackEntry(URL link, String loc) throws IOException {
	        this.url = link;
	        this.location = loc;
	        this.reader = new LineNumberReader(new InputStreamReader(url.openStream(), "ISO-8859-1"));
	        this.reader.setLineNumber(1);
	    }

	    public String getLocation() {
	        return location;
	    }

	    public URL getURL() {
	        return url;
	    }

	    public LineNumberReader getReader() {
	        return reader;
	    }

	    public String toString() {
	        return "IncludeStackEntry[url=" + url + ", location=" + location
	            + ", line=" + reader.getLineNumber() + "]";
	    }
	}


