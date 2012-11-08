/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)FileUtilities.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.common;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.SyncFailedException;
import java.io.UnsupportedEncodingException;

/**
 * @author graj
 *
 */
public class FileUtilities {

	/**
	 * 
	 */
	public FileUtilities() {
		// TODO Auto-generated constructor stub
	}
	
	/**
	 * 
	 * @param folderName
	 * @return
	 */
	public static File createFolder(String folderName) {
		File folder = new File(folderName);
		if(folder.exists() == false) {
			try {
				folder.mkdirs();
			} catch(RuntimeException exception) {
				exception.printStackTrace();
			}
		}
		System.out.println("Path is: "+folder.getAbsolutePath());
		return folder;
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
            throws IllegalArgumentException, IOException {
        if (aFile == null) {
            throw new IllegalArgumentException("File should not be null.");
        }
        /*
        if (true == aFile.isDirectory()) {
            throw new IllegalArgumentException("Should not be a directory: "
                    + aFile);
        }
        if (false == aFile.canWrite()) {
            throw new IllegalArgumentException("File cannot be written: "
                    + aFile);
        }
        */

        // declared here only to make visible to finally clause; 
        // generic reference
        FileOutputStream outputStream = null;
        FileDescriptor fileDescriptor = null;
        try {
            // Open or create the output file
            outputStream = new FileOutputStream(aFile);
            fileDescriptor = outputStream.getFD();
        
            // Write some data to the stream
            outputStream.write(aContents.getBytes());
        } finally {
            // flush and close both "output" and its underlying FileWriter
            if (outputStream != null) {
                // Flush the data from the streams and writers into system buffers.
                // The data may or may not be written to disk.
                outputStream.flush();
                if(fileDescriptor != null) {
	                // Block until the system buffers have been written to disk.
	                // After this method returns, the data is guaranteed to have
	                // been written to disk.
	                fileDescriptor.sync();
                }
                outputStream.close();
            }
        }
    }

    /**
	 * Writes data to a file
	 * 
     * @param file
     * @param content
     * @throws FileNotFoundException
     * @throws UnsupportedEncodingException
     * @throws IOException
     */
	public static void writeToFile(File file, String content)
	throws FileNotFoundException, UnsupportedEncodingException, SyncFailedException, IOException {
		writeToFile(file.getAbsolutePath(), content, "UTF-8");
	}    

    /**
	 * Writes data to a file
	 * 
     * @param file
     * @param content
     * @param encoding
     * @throws FileNotFoundException
     * @throws UnsupportedEncodingException
     * @throws IOException
     */
	public static void writeToFile(File file, String content, String encoding)
	throws FileNotFoundException, UnsupportedEncodingException, SyncFailedException, IOException {
		writeToFile(file.getAbsolutePath(), content, encoding);
	}    
	
    
    /**
	 * Writes data to a file
	 * 
     * @param fileLocation
     * @param content
     * @throws FileNotFoundException
     * @throws UnsupportedEncodingException
     * @throws IOException
     */
	public static void writeToFile(String fileLocation, String content)
	throws FileNotFoundException, UnsupportedEncodingException, SyncFailedException, IOException {
		writeToFile(fileLocation, content, "UTF-8");
	}    
    

	/**
	 * Writes data to a file
	 *  
	 * @param fileLocation
	 * @param content
	 * @param encoding
	 * @throws FileNotFoundException
	 * @throws UnsupportedEncodingException
	 * @throws IOException
	 */
	public static void writeToFile(String fileLocation, String content, String encoding)
		throws FileNotFoundException, UnsupportedEncodingException, SyncFailedException, IOException {
			FileOutputStream fileOutputStream = null;
			FileDescriptor fileDescriptor = null;
			try {
				fileOutputStream = new FileOutputStream(fileLocation);
			    fileDescriptor = fileOutputStream.getFD();
				
				IOUtilities.copy(content.getBytes("UTF-8"), fileOutputStream);
			} finally {
			    // flush and close both "output" and its underlying FileWriter
			    if (fileOutputStream != null) {
			        // Flush the data from the streams and writers into system buffers.
			        // The data may or may not be written to disk.
			    	// copy() already calls flush()
			    	// fileOutputStream.flush();
			        if(fileDescriptor != null) {
			            // Block until the system buffers have been written to disk.
			            // After this method returns, the data is guaranteed to have
			            // been written to disk.
			            fileDescriptor.sync();
			        }
			        fileOutputStream.close();
//			        try {
//						Thread.sleep(1000L);
//					} catch (InterruptedException e) {
//						// TODO Auto-generated catch block
//						e.printStackTrace();
//					}
			    }
			}
	}    
	

	public static void copyFile(File from, File to) throws IOException {
		int bytes;
		long length;
		long fileSize;

		BufferedInputStream in = null;
		BufferedOutputStream out = null;

		try {
			in = new BufferedInputStream(new FileInputStream(from));
		} catch (IOException ex) {
			throw new IOException(
					"FileUtilities.copyFile: opening input stream '"
							+ from.getPath() + "', " + ex.getMessage());
		}

		try {
			out = new BufferedOutputStream(new FileOutputStream(to));
		} catch (Exception ex) {
			try {
				in.close();
			} catch (IOException ex1) {
			}
			throw new IOException(
					"FileUtilities.copyFile: opening output stream '"
							+ to.getPath() + "', " + ex.getMessage());
		}

		byte[] buffer;
		buffer = new byte[8192];
		fileSize = from.length();

		for (length = fileSize; length > 0;) {
			bytes = (int) (length > 8192 ? 8192 : length);

			try {
				bytes = in.read(buffer, 0, bytes);
			} catch (IOException ex) {
				try {
					in.close();
					out.close();
				} catch (IOException ex1) {
				}
				throw new IOException(
						"FileUtilities.copyFile: reading input stream, "
								+ ex.getMessage());
			}

			if (bytes < 0)
				break;

			length -= bytes;

			try {
				out.write(buffer, 0, bytes);
			} catch (IOException ex) {
				try {
					in.close();
					out.close();
				} catch (IOException ex1) {
				}
				throw new IOException(
						"FileUtilities.copyFile: writing output stream, "
								+ ex.getMessage());
			}
		}

		try {
			in.close();
			out.close();
		} catch (IOException ex) {
			throw new IOException(
					"FileUtilities.copyFile: closing file streams, "
							+ ex.getMessage());
		}
	}

	public static boolean fileEqualsExtension(String fileName, String extension) {
		boolean result = false;

		int fnLen = fileName.length();
		int exLen = extension.length();

		if (fnLen > exLen) {
			String fileSuffix = fileName.substring(fnLen - exLen);

			if (FileUtilities.caseSensitivePathNames()) {
				result = fileSuffix.equals(extension);
			} else {
				result = fileSuffix.equalsIgnoreCase(extension);
			}
		}

		return result;
	}

	static public boolean caseSensitivePathNames() {
		boolean result = true;

		String osname = System.getProperty("os.name");

		if (osname != null) {
			if (osname.startsWith("macos"))
				result = false;
			else if (osname.startsWith("Windows"))
				result = false;
		}

		return result;
	}

	/**
	 * Determines if a filename matches a 'globbing' pattern.
	 * The pattern can contain the following special symbols:
	 * <ul>
	 * <li> * - Matches zero or more of any character
	 * <li> ? - Matches exactly one of any character
	 * <li> [...] - Matches one of any character in the list or range
	 * </ul>
	 * 
	 * @param fileName The name of the file to check.
	 * @param matchExpr The expression to check against.
	 * @return If the file name matches the expression, true, else false.
	 */
	public static boolean isPatternString(String pattern) {
		if (pattern.indexOf("*") >= 0)
			return true;
		if (pattern.indexOf("?") >= 0)
			return true;

		int index = pattern.indexOf("[");
		if ((index >= 0) && (pattern.indexOf("]") > index + 1))
			return true;

		return false;
	}

	public static boolean matchPattern(String fileName, String pattern) {
		return FileUtilities.recurseMatchPattern(fileName, pattern, 0, 0);
	}

	/**
	 * An internal routine to implement expression matching.
	 * This routine is based on a self-recursive algorithm.
	 * 
	 * @param string The string to be compared.
	 * @param pattern The expression to compare <em>string to.
	 * @param sIdx The index of where we are in <em>string.
	 * @param pIdx The index of where we are in <em>pattern.
	 * @return True if <em>string matched pattern, else false.
	 */
	private static boolean recurseMatchPattern(String string, String pattern,
			int sIdx, int pIdx) {
		int pLen = pattern.length();
		int sLen = string.length();

		for (;;) {
			if (pIdx >= pLen) {
				if (sIdx >= sLen)
					return true;
				else
					return false;
			}

			if (sIdx >= sLen && pattern.charAt(pIdx) != '*') {
				return false;
			}

			// Check for a '*' as the next pattern char.
			// This is handled by a recursive call for
			// each postfix of the name.
			if (pattern.charAt(pIdx) == '*') {
				if (++pIdx >= pLen)
					return true;

				for (;;) {
					if (FileUtilities.recurseMatchPattern(string, pattern,
							sIdx, pIdx))
						return true;

					if (sIdx >= sLen)
						return false;

					++sIdx;
				}
			}

			// Check for '?' as the next pattern char.
			// This matches the current character.
			if (pattern.charAt(pIdx) == '?') {
				++pIdx;
				++sIdx;
				continue;
			}

			// Check for '[' as the next pattern char.
			// This is a list of acceptable characters,
			// which can include character ranges.
			if (pattern.charAt(pIdx) == '[') {
				for (++pIdx;; ++pIdx) {
					if (pIdx >= pLen || pattern.charAt(pIdx) == ']')
						return false;

					if (pattern.charAt(pIdx) == string.charAt(sIdx))
						break;

					if (pIdx < (pLen - 1) && pattern.charAt(pIdx + 1) == '-') {
						if (pIdx >= (pLen - 2))
							return false;

						char chStr = string.charAt(sIdx);
						char chPtn = pattern.charAt(pIdx);
						char chPtn2 = pattern.charAt(pIdx + 2);

						if ((chPtn <= chStr) && (chPtn2 >= chStr))
							break;

						if ((chPtn >= chStr) && (chPtn2 <= chStr))
							break;

						pIdx += 2;
					}
				}

				for (; pattern.charAt(pIdx) != ']'; ++pIdx) {
					if (pIdx >= pLen) {
						--pIdx;
						break;
					}
				}

				++pIdx;
				++sIdx;
				continue;
			}

			// Check for backslash escapes
			// We just skip over them to match the next char.
			if (pattern.charAt(pIdx) == '\\') {
				if (++pIdx >= pLen)
					return false;
			}

			if (pIdx < pLen && sIdx < sLen)
				if (pattern.charAt(pIdx) != string.charAt(sIdx))
					return false;

			++pIdx;
			++sIdx;
		}
	}

	public static String getUserHomeDirectory() {
		String userDirName = System.getProperty("user.home", null);

		if (userDirName == null) {
			userDirName = System.getProperty("user.dir", null);
		}

		return userDirName;
	}

    /**
     * Simple test harness.
     */
    public static void main(String... aArguments) throws IOException {
        File testFile = new File("/tmp/blah.txt");
//        testFile.createNewFile();

        setContents(testFile, "Gopalan says hello.");
        //System.out.println("New file contents: " + getContents(testFile));
    }

}
