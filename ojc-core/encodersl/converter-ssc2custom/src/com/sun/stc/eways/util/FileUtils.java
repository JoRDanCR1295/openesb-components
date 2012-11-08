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
 * @(#)FileUtils.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.eways.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

/**
 * This class encapsulates handy file methods
 *
 * @author Scott Steadman ssteadman@seebeyond.com
 */
public class FileUtils {

    /**
     * private constructor to prevent instantiation
     */
    private FileUtils (){}

    /**
	* Writes a string to the specified file.
	* <p>
	* @param _fileName The name of the file.
	* @param _string The string to be written to the file.
	* @return None.
	* @exception IOException Thrown if the attempt to write to the file fails.
	* @see #write(String,String,String)
	* @see #write(String,byte[])
	* @include
	*/
	public static void write (String _fileName, String _string) throws IOException {
        FileUtils.write(_fileName, _string, "UTF8");
    }

    /**
	* Writes a string to a file using the specified encoding format.
	* <p>
	* @param _fileName The name of the file.
	* @param _string The string to be written to the file.
	* @param _encoding The encoding type.
	* @return None.
	* @exception IOException Thrown if the attempt to write to the file fails.
	* @see #write(String,String)
	* @see #write(String,byte[])
	* @include
	*/
	public static void write (String _fileName, String _string, String _encoding) throws IOException {
        FileUtils.write(_fileName, _string.getBytes(_encoding));
    }

    /**
	* Writes a byte array to the specified file.
	* <p>
	* @param _fileName The name of the file.
	* @param _bytes The bytes to be written to the file.
	* @return None.
	* @exception IOException Thrown if the attempt to write to the file fails.
	* @see #write(String,String)
	* @see #write(String,String,String)
	* @include
	*/
	public static void write (String _fileName, byte[] _bytes) throws IOException {
        FileOutputStream fos = new FileOutputStream(_fileName);
        fos.write(_bytes);
        fos.close();
    }

    /**
	* Reads a file into a byte array. <p><i><b>Note:</b> Large files require a large amount of available memory.</i>
	* <p>
	* @param _fileName The name of the file.
	* @return <CODE>byte[]</CODE> - The contents of the file as a byte array.
	* @exception IOException Thrown if the attempt to read the file fails.
	* @see #readString(String)
	* @see #readString(String,String)
	* @include
	*/
	public static byte[] readBytes (String _fileName) throws IOException {
        FileInputStream fis = new FileInputStream(_fileName);
        byte[] bytes = new byte[fis.available()];
        fis.read(bytes);
        fis.close();
        return bytes;
    }

	/**
	* Reads the contents of a file and converts the contents to a string. The file is assumed to be in UTF-8 encoding format.
	* <p>
	* @param _fileName The name of the file.
	* @return <CODE>String</CODE> - The contents of the file converted to a string.
	* @exception IOException Thrown if the attempt to read the file fails.
	* @see #readBytes
	* @see #readString(String,String)
	*/
	public static String readString (String _fileName) throws IOException {
        return FileUtils.readString(_fileName, "UTF8");
    }

    /**
	* Reads the contents of a file and converts the contents to a string using the specified encoding format.
	* <p>
	* @param _fileName The name of the file.
	* @param _encoding The encoding format to use.
	* @return <CODE>String</CODE> - The contents of the file converted to a string.
	* @exception IOException Thrown if the attempt to read the file fails.
	* @see #readBytes
	* @see #readString(String)
	* @include
	*/
	public static String readString (String _fileName, String _encoding) throws IOException {
        return new String(FileUtils.readBytes(_fileName), _encoding);
    }

    /**
	* Tests to see if the specified file exists.
	* <p>
	* @param _fileName The name of the file.
	* @return <CODE>boolean</CODE> - Returns <code><b>true</code></b> if the file exists. Otherwise, returns <code><b>false</code></b>.
	* <DT><B>Throws:</B><DD>None.
	* @include
	*/
	public static boolean exists (String _fileName) {
        try {
            return new File(_fileName).exists();
        } catch(Exception ex) {
            return false;
        }
    }
}
