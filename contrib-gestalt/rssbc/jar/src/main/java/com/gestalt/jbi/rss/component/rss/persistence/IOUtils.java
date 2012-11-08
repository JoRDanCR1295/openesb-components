/**
 *   rss-binding-component - RSS Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.rss.component.rss.persistence;

import java.io.*;

import java.util.logging.Logger;


/**
 * class for reading and writing files
 */
public class IOUtils {
    private static Logger log = Logger.getLogger(IOUtils.class.getName());

    public static String readInputStream(InputStream in)
        throws IOException {
        StringBuilder sb = new StringBuilder();
        BufferedReader buff = new BufferedReader(new InputStreamReader(in));
        String s;

        try {
            while (null != (s = buff.readLine())) {
                sb.append(s).append("\n");
            }
        } finally {
            in.close();
        }

        return sb.toString().trim();
    }

    public static String readFile(File file) throws IOException {
        return readInputStream(new FileInputStream(file));
    }

    public static void writeToFile(String msg, File file)
        throws IOException {
        PrintWriter out = new PrintWriter(new BufferedWriter(
                    new FileWriter(file)));

        try {
            out.write(msg);
        } finally {
            out.flush();
            out.close();
        }
    }

    public static void storeObject(Object o, File file) {
        try {
            OutputStream out = new FileOutputStream(file);
            ObjectOutputStream oos = new ObjectOutputStream(out);

            try {
                oos.writeObject(o);
            } finally {
                oos.close();
            }
        } catch (IOException e) {
            log.warning("UNABLE TO STORE OBJECT: " + e.getMessage());
        }
    }

    public static Object loadObject(File file) {
        Object o = null;

        try {
            InputStream in = new FileInputStream(file);
            ObjectInputStream ois = new ObjectInputStream(in);

            try {
                o = ois.readObject();
            } finally {
                ois.close();
            }
        } catch (Exception e) {
            log.warning("UNABLE TO LOAD OBJECT");
        }

        return o;
    }
}
