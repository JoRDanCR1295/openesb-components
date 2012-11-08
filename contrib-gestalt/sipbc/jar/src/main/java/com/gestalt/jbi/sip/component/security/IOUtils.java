package com.gestalt.jbi.sip.component.security;

import com.sun.jbi.internationalization.Messages;

import java.io.*;

import java.util.logging.Logger;
import java.util.logging.Level;


/**
 * class for reading and writing files
 * todo this class is duplicated in RSSBC...
 * move to jbi common runtime?
 */
public class IOUtils {
    private static Logger log = Messages.getLogger(IOUtils.class);
    private static Messages messages = Messages.getMessages(IOUtils.class);

    public static String readInputStream(InputStream in)
        throws IOException {
        StringBuilder sb = new StringBuilder();
        BufferedReader buff = new BufferedReader(new InputStreamReader(in));
        String s;

        try {
            for (s = buff.readLine(); null != s; s = buff.readLine()) {
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
            log.log(Level.WARNING,messages.getString("SIPBC-W00502.unableToStoreObject"),e);
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
            log.log(Level.WARNING,messages.getString("SIPBC-W00503.unableToLoadObject"),e);
        }

        return o;
    }
}
