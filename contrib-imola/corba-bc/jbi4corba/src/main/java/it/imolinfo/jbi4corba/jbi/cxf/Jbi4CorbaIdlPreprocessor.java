/****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.cxf;

import java.io.IOException;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

import org.apache.cxf.tools.corba.idlpreprocessor.DefineState;
import org.apache.cxf.tools.corba.idlpreprocessor.IncludeResolver;
import org.apache.cxf.tools.corba.idlpreprocessor.PreprocessingException;

/**
 * A Reader that implements the #include functionality of the preprocessor.
 * Starting from one URL, it generates one stream of characters by tracking
 * #defines, #ifdefs, etc. and following #includes accordingly.
 * 
 * <p>
 * This reader augments the stream with <a
 * href="http://gcc.gnu.org/onlinedocs/gcc-3.2.3/cpp/Preprocessor-Output.html">
 * location information</a> when the source URL is switched. This improves error
 * reporting (with correct file and linenumber information) in the subsequent
 * compilation steps like IDL parsing and also allows the implentation of code
 * generation options like the -emitAll flag available in the JDK idlj tool.
 * </p>
 */
public final class Jbi4CorbaIdlPreprocessor /*extends Reader*/ {

	/**
	 * Maximum depth of {@link #includeStack} to prevent infinite recursion.
	 */
	private static final int MAX_INCLUDE_DEPTH = 64;

	/**
	 * GNU standard preprocessor output flag for signalling a new file.
	 * 
	 * @see http://gcc.gnu.org/onlinedocs/gcc-3.2.3/cpp/Preprocessor-Output.html
	 */
	private static final char PUSH = '1';

	/**
	 * GNU standard preprocessor output flag for signalling returning to a file.
	 * 
	 * @see http://gcc.gnu.org/onlinedocs/gcc-3.2.3/cpp/Preprocessor-Output.html
	 */
	private static final char POP = '2';

	private static final String LF = System.getProperty("line.separator");

	private final IncludeResolver includeResolver;

	private final Stack<Jbi4CorbaIncludeStackEntry> includeStack = new Stack<Jbi4CorbaIncludeStackEntry>();
        
        private final Set<String> includeSet=new HashSet<String>();
        
        private final Set<URL> idlSet=new HashSet<URL>();

	/**
	 * Stack of Booleans, corresponding to nested 'if' preprocessor directives.
	 * The top of the stack signals whether the current idl code is skipped.
	 * 
	 * @see #skips()
	 */
	private final Stack<Boolean> ifStack = new Stack<Boolean>();

	private final DefineState defineState;

	private int readPos;

	private static final String ORBFNAME = "orb.idl";
	private static final String IRFNAME = "ir.idl";

	/**
	 * Creates a new IncludeReader.
	 * 
	 * @param startURL
	 * @param startLocation
	 * @param includeResolver
	 * @param defineState
	 * @throws IOException
	 */
	public Jbi4CorbaIdlPreprocessor(URL startURL, String startLocation,
			IncludeResolver resolver, DefineState state) throws IOException {
		this.includeResolver = resolver;
		this.defineState = state;
		pushInclude(startURL, startLocation);
		fillBuffer();
	}

    
        

	/**
	 * @param url
	 * @throws IOException
	 */
	private void pushInclude(URL url, String location) throws IOException {
		final Jbi4CorbaIncludeStackEntry includeStackEntry = new Jbi4CorbaIncludeStackEntry(
				url, location);
		includeStack.push(includeStackEntry);
		
	}



	private void fillBuffer() throws IOException {
		while (!includeStack.isEmpty()) {
			LineNumberReader reader = getReader();
			final int lineNo = reader.getLineNumber();
			String line = reader.readLine();

			if (line == null) {
				popInclude();
				continue;
			}
			
			if (!line.trim().startsWith("#")) {				
				continue;
			}

			final Jbi4CorbaIncludeStackEntry ise = includeStack.peek();
			line = line.trim();

			if (line.startsWith("#include") && (line.contains(ORBFNAME) || line.contains(IRFNAME))) {
                            
                            if(!idlSet.contains(getCurrentURL())){
                                idlSet.add(getCurrentURL());
                            }
                            
			} 
                        
                        else if (line.startsWith("#include")) {
				handleInclude(line, lineNo, ise);
                        }
			
		}
	}
	

	

	private void handleInclude(String line, int lineNo,
			final Jbi4CorbaIncludeStackEntry ise) throws IOException {

		if (skips()) {
			
			return;
		}

		if (includeStack.size() >= MAX_INCLUDE_DEPTH) {
			throw new PreprocessingException(
					"more than "
							+ MAX_INCLUDE_DEPTH
							+ " nested #includes - assuming infinite recursion, aborting",
					ise.getURL(), lineNo);
		}

		String arg = line.replaceFirst("#include", "").trim();		
		if (arg.length() == 0) {
			throw new PreprocessingException("#include without an argument",
					ise.getURL(), lineNo);
		}

		char first = arg.charAt(0);
		final int lastIdx = arg.length() - 1;
		char last = arg.charAt(lastIdx);
		if (arg.length() < 3 || !(first == '<' && last == '>')
				&& !(first == '"' && last == '"')) {
			throw new PreprocessingException(
					"argument for '#include' must be enclosed in '< >' or '\" \"'",
					ise.getURL(), lineNo);
		}
		String spec = arg.substring(1, lastIdx);
		//File URL
		URL include = (first == '<') ? includeResolver.findSystemInclude(spec)
				: includeResolver.findUserInclude(spec);

		if (include == null) {
			throw new PreprocessingException("unable to resolve include '"
					+ spec + "'", ise.getURL(), lineNo);
		}
                
		pushInclude(include, spec);
                
                if(!includeSet.contains(spec)){
                     includeSet.add(spec);
                }
               
	}

	private void popInclude() throws IOException {
		final Jbi4CorbaIncludeStackEntry poppedStackEntry = includeStack.pop();
		try {
			if (includeStack.size() > 0) {
				final Jbi4CorbaIncludeStackEntry newTopEntry = includeStack
						.peek();
				final LineNumberReader reader = getReader();
//				final int lineNumber = reader.getLineNumber();
//				final String location = newTopEntry.getLocation();                        				
			}
		} finally {
			poppedStackEntry.getReader().close();
		}
	}

	private boolean skips() {
		if (ifStack.isEmpty()) {
			return false;
		}

		return ifStack.peek();
	}


	private LineNumberReader getReader() {
		Jbi4CorbaIncludeStackEntry topOfStack = includeStack.peek();
		return topOfStack.getReader();
	}
        
        private URL getCurrentURL(){
                Jbi4CorbaIncludeStackEntry topOfStack = includeStack.peek();
                return topOfStack.getURL();
        }


        
        /**
         * Return the set that contains all the information for the inclusion of idl in the wsdl
         */
        public Set<String> getIncludesPath() {
            return includeSet;
        }
        
        /**
         * Return all idl Url that contain #include orb.idl ir.idl
         */
        public Set<URL> getIdlURL(){
            return idlSet;
        }
        
}
