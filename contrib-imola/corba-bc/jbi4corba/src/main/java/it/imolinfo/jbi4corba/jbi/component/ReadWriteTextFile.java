 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.component;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.Messages;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;

/**
 * 
 * @author graj
 */
public class ReadWriteTextFile {

	//	 The logger. 
    private static final Logger LOG = 
    	LoggerFactory.getLogger(ReadWriteTextFile.class);
    private static final Messages MESSAGES = 
    	Messages.getMessages(ReadWriteTextFile.class);
	
    /**
     * Fetch the entire contents of a text file, and return it in a String. This
     * style of implementation does not throw Exceptions to the caller.
     * 
     * @param aFile
     *            is a file which already exists and can be read.
     */
    static public String getContents(File aFile) {

        // ...checks on aFile are elided
        StringBuffer contents = new StringBuffer();

        // declared here only to make visible to finally clause
        BufferedReader input = null;
        try {
            // use buffering, reading one line at a time
            // FileReader always assumes default encoding is OK!
            input = new BufferedReader(new FileReader(aFile));
            String line = null; // not declared within while loop
            /*
             * readLine is a bit quirky : it returns the content of a line MINUS
             * the newline. it returns null only for the END of the stream. it
             * returns an empty String if two newlines appear in a row.
             */
            while ((line = input.readLine()) != null) {
                contents.append(line);
                contents.append(System.getProperty("line.separator"));
            }
        } catch (FileNotFoundException ex) {
            ex.printStackTrace();
        } catch (IOException ex) {
            ex.printStackTrace();
        } finally {
            try {
                if (input != null) {
                    // flush and close both "input" and its underlying
                    // FileReader
                    input.close();
                }
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
        return contents.toString();
    }

    /**
     * Change the contents of text file in its entirety, overwriting any
     * existing text.
     * 
     * This style of implementation throws all exceptions to the caller.
     * 
     * @param aFile
     *            is an existing file which can be written to.
     * @throws IllegalArgumentException
     *             if param does not comply.
     * @throws FileNotFoundException
     *             if the file does not exist.
     * @throws IOException
     *             if problem encountered during write.
     */
    static public void setContents(File aFile, String aContents)
            throws FileNotFoundException, IOException {
        if (aFile == null) {
        	String msg=MESSAGES.getString("CRB000226_File_should_not_be_null");
            LOG.error(msg);
            throw new IllegalArgumentException(msg);

        }
        if (!aFile.exists()) {
        	String msg=MESSAGES.getString("CRB000227_File_does_not_exist", new Object[] {aFile});
            LOG.error(msg);
            throw new FileNotFoundException(msg);
        }
        if (!aFile.isFile()) {
        	String msg=MESSAGES.getString("CRB000228_Should_not_be_a_directory", new Object[] {aFile});
            LOG.error(msg);
            throw new IllegalArgumentException(msg);
        }
        if (!aFile.canWrite()) {
        	String msg=MESSAGES.getString("CRB000229_File_cannot_be_written", new Object[] {aFile});
            LOG.error(msg);
            throw new IllegalArgumentException(msg);
        }

        // declared here only to make visible to finally clause; generic
        // reference
        Writer output = null;
        try {
            // use buffering
            // FileWriter always assumes default encoding is OK!
            output = new BufferedWriter(new FileWriter(aFile));
            output.write(aContents);
        } finally {
            // flush and close both "output" and its underlying FileWriter
            if (output != null)
                output.close();
        }
    }

    /**
     * Simple test harness.
     */
    public static void main(String... aArguments) throws IOException {
        File testFile = new File("/tmp/blah.txt");
        LOG.debug("Original file contents: " + getContents(testFile));
        setContents(testFile, "Gopalan says hello.");
        LOG.debug("New file contents: " + getContents(testFile));
    }
}