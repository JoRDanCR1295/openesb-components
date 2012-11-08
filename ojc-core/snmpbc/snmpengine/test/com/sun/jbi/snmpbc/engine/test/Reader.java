
package com.sun.jbi.snmpbc.engine.test;

import com.sun.org.apache.xerces.internal.impl.dv.util.Base64;

import java.net.DatagramPacket;
import java.net.DatagramSocket;

public class Reader {
    public static final int PORT = 5541;
    
    String TESTTRAP = "MDsCAQAEBnB1YmxpY6QuBggrBgEEAQMBAUAEfwAAAQIBAwIBAEMBADATMBEGCCsGAQIBAQEABAVoZWxsbw==";

    public static void main(String argv[]) {
        try {
            byte[] inbuf = new byte[65536];
            DatagramSocket socketin = new DatagramSocket(PORT);
            DatagramPacket packetin = new DatagramPacket(inbuf, inbuf.length);
            
            for (;;) {
                socketin.receive(packetin);
                byte[] resized = new byte[packetin.getLength()];
                System.arraycopy(inbuf, 0, resized, 0, resized.length);
                String encoded = Base64.encode(resized);
                System.out.println(encoded);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
