/* *************************************************************************
 *
 *          Copyright (c) 2002, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/

package com.sun.jbi.httpsoapbc.util;

import java.io.*;
import java.net.Socket;
import java.util.zip.CRC32;
import java.net.ServerSocket;
import java.util.Properties;

/**
 * A collection of tools and utilities
 *
 * @author fkieviet
 */
public class IOTools {

    /**
     * Closes a stream without exceptions
     */
    public static void safeClose(InputStream in) {
        if (in != null) {
            try {
                in.close();
            } catch (Exception ex) {
                // ignore
            }
        }
    }

    /**
     * Closes a stream without exceptions
     */
    public static void safeClose(Reader in) {
        if (in != null) {
            try {
                in.close();
            } catch (Exception ex) {
                // ignore
            }
        }
    }

    /**
     * Closes a stream without exceptions
     */
    public static void safeClose(OutputStream out) {
        if (out != null) {
            try {
                out.close();
            } catch (Exception ex) {
                // ignore
            }
        }
    }

    /**
     * Closes a stream without exceptions
     */
    public static void safeClose(ServerSocket s) {
        if (s != null) {
            try {
                s.close();
            } catch (Exception ex) {
                // ignore
            }
        }
    }

    /**
     * Closes a socket exceptions
     */
    public static void safeClose(Socket s) {
        if (s != null) {
            try {
                s.close();
            } catch (Exception ex) {
                // ignore
            }
        }
    }

    public static String readLine(File f) {
        if (f == null) {
            return null;
        }
        
        BufferedReader r = null;
        try {
            r = new BufferedReader(new FileReader(f));
            return r.readLine();
        } catch (IOException e) {
            throw new RuntimeException("Failed to read line from " + f.getAbsolutePath() + ": " + e, e);
        } finally {
            safeClose(r);
        }
    }
    
    public static String serialize(Properties p) {
        try {
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            p.store(out, "");
            return out.toString();
        } catch (Exception e) {
            throw new RuntimeException("Failed to serialize properties: " + e, e);
        }
    }

    /**
     * Reads a stream completely into a string, max 8k
     */
    private static String read(InputStream inp) {
        try {
            StringBuffer ret = new StringBuffer();
            InputStreamReader reader = new InputStreamReader(inp);
            char[] buf = new char[8192];
            for (;;) {
                int n = reader.read(buf, 0, buf.length);
                if (n <= 0) {
                    break;
                }
                ret.append(buf, 0, n);
            }
            return ret.toString();
        } catch (Exception ex) {
return "READ ERROR: " + ex;            
//            throw new RuntimeException("Could not internalize stream: " + ex, ex);
        }
    }

}
