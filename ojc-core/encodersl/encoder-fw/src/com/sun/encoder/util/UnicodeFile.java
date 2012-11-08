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
 * @(#)UnicodeFile.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

/**
 * The UnicodeFile class contains common code to use on UTF-8 encoded files.
 * This code was factored out because a lot of eGate code needs to use these
 * for portability and platform independence, as most Java I/O methods assume
 * a locale-specific platform default encoding that may not support the full
 * Unicode character set.
 *
 * @author Michael Libourel
 */
public final class UnicodeFile {

    public static final String ENC = "UTF-8";
    static final int CHUNK = 1024;

    /**
     * Creates new UTF-8 encoded, buffered input stream reader.
     *
     * @param file  the input file
     * @return the stream
     * @throws FileNotFoundException file not found.
     */
    public static InputStreamReader makeInputReader(File file)
        throws FileNotFoundException {
        try {
            return new InputStreamReader(
                new BufferedInputStream(new FileInputStream(file)), ENC);
        } catch (UnsupportedEncodingException ue) {
            throw new RuntimeException("broken JRE, no " + ENC);
        }
    }

    /**
     * Creates new UTF-8 encoded, buffered input stream reader.
     *
     * @param in  the input stream
     * @return the stream
     */
    public static InputStreamReader makeInputReader(InputStream in) {
        try {
            return new InputStreamReader(new BufferedInputStream(in), ENC);
        } catch (UnsupportedEncodingException ue) {
            throw new RuntimeException("broken JRE, no " + ENC);
        }
    }

    /**
     * Creates new UTF-8 encoded, buffered output stream writer.
     *
     * @param file  the output file
     * @return the stream
     * @throws FileNotFoundException file not found.
     */
    public static OutputStreamWriter makeOutputWriter(File file)
        throws FileNotFoundException {
        try {
            return new OutputStreamWriter(
                new BufferedOutputStream(new FileOutputStream(file)), ENC);
        } catch (UnsupportedEncodingException ue) {
            throw new RuntimeException("broken JRE, no " + ENC);
        }
    }

    /**
     * Reads the contents of the given UTF-8 encoded file as a string.
     *
     * @param file  the input file
     * @return the contents
     * @throws IOException file i-o problem.
     */
    public static String getText(File file)
        throws IOException {
        if (file == null) {
            throw new NullPointerException("No file specified.");
        }
        StringBuffer sb = new StringBuffer((int) file.length());
        Reader in = makeInputReader(file);
        int count;
        char[] buf = new char[CHUNK];
        while ((count = in.read(buf)) > 0) {
            sb.append(buf, 0, count);
        }
        in.close();
        return sb.toString();
    }

    /**
     * Reads the contents of the given UTF-8 encoded files as a string array.
     *
     * @param pathPattern the path pattern denotes a list of files. Currently
     *                    the only recognized token is '%i%', when being put
     *                    inside a path, indicates that the file paths shall be
     *                    generated each time by replacing the token with an
     *                    empty string and then a series of integers starting
     *                    from one. E.g., path pattern '/a/b%i%.txt' shall be
     *                    intepretated as a series of paths: '/a/b.txt',
     *                    '/a/b1.txt', '/a/b2.txt', '/a/b3.txt',...
     * @return the contents
     * @throws IOException file i-o problem.
     */
    public static String[] getTexts(String pathPattern)
            throws IOException {
        if (pathPattern == null) {
            throw new NullPointerException("No path pattern specified.");
        }
        if (pathPattern.indexOf("%i%") == -1) {
            return new String[]{getText(new File(pathPattern))};
        }
        List<String> contentList = new ArrayList<String>();
        File file;
        int i = 0;
        String replacement = "";
        for (;;) {
            if (i > 0) {
                replacement = String.valueOf(i);
            }
            file = new File(pathPattern.replaceAll("\\%i\\%", replacement));
            if (!file.exists()) {
                break;
            }
            contentList.add(getText(file));
            i++;
        }
        return contentList.toArray(new String[0]);
    }

    /**
     * Writes the string as a UTF-8 encoded file.
     *
     * @param file  the output file
     * @param text  the contents
     * @throws IOException file i-o problem.
     */
    public static void setText(File file, String text)
        throws IOException {
        if (file == null) {
            throw new NullPointerException("no file");
        }
        if (text == null) {
            throw new NullPointerException("no text");
        }
        Writer out = makeOutputWriter(file);
        out.write(text);
        out.close();
    }
}
