/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.zaz.ssapi.protocol.tn3270.test;

import com.zaz.ssapi.protocol.tn3270.*;
import java.util.Properties;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 *
 * @author rchen
 */
public class TN3270Test extends TestCase {

    TN3270 tn3270;

    public TN3270Test(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        tn3270 = new TN3270();
        try {
            tn3270.ConnectPresentationSpace("lyssa", "23");
        } catch (Exception ex) {
            System.out.println("Connection falied,  will abort all tests");
            tn3270 = null;
            
        }

    }

    protected void tearDown() throws Exception {
        tn3270 = null;
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(TN3270Test.class);

        return suite;
    }

    public void testSomeCommands() throws Exception {
        if (tn3270 == null)
            return;

        tn3270.CopyPresentationSpace();

        tn3270.SetAction(125);

        tn3270.SetCursor(1781);

        tn3270.CopyStringToField(1, "TSO");

        tn3270.SendPresentationSpace();

        tn3270.CopyPresentationSpace();

        tn3270.SetAction(125);

        tn3270.SetCursor(84);

        tn3270.CopyStringToField(1, "P390");
        tn3270.SendPresentationSpace();

        tn3270.CopyPresentationSpace();

        tn3270.SetAction(125);

        tn3270.SetCursor(586);
        tn3270.CopyStringToField(12, "tequila");
        tn3270.SendPresentationSpace();

        tn3270.CopyPresentationSpace();

        tn3270.SetAction(125);

        tn3270.SetCursor(805);

        tn3270.SendPresentationSpace();

        tn3270.CopyPresentationSpace();

        tn3270.SetAction(125);

        tn3270.SetCursor(253);

        tn3270.SendPresentationSpace();

        tn3270.CopyPresentationSpace();

        tn3270.SetAction(125);

        tn3270.SetCursor(254);

        tn3270.CopyStringToField(14, "r");

        tn3270.SendPresentationSpace();

        tn3270.CopyPresentationSpace();

        tn3270.SetAction(125);

        tn3270.SetCursor(94);

        tn3270.CopyStringToField(3, "4");

        tn3270.SendPresentationSpace();

        tn3270.CopyPresentationSpace();

        tn3270.SetAction(125);

        tn3270.SetCursor(94);

        tn3270.CopyStringToField(2, "d");

        tn3270.SendPresentationSpace();

        tn3270.CopyPresentationSpace();

        tn3270.SetAction(125);

        tn3270.SetCursor(1463);
        tn3270.CopyStringToField(37, "qa003");

        tn3270.SendPresentationSpace();
        tn3270.CopyPresentationSpace();

        tn3270.SetAction(125);

        tn3270.SetCursor(325);

        tn3270.CopyStringToField(7, "x");

        tn3270.SendPresentationSpace();

        tn3270.CopyPresentationSpace();

        tn3270.SetAction(243);

        tn3270.SetCursor(0);

        tn3270.BackupPresentationSpace();

        tn3270.SendPresentationSpace();

        tn3270.CopyPresentationSpace();

        tn3270.SetAction(243);

        tn3270.SetCursor(93);

        tn3270.SendPresentationSpace();

        tn3270.CopyPresentationSpace();

        tn3270.SetAction(243);

        tn3270.SetCursor(93);

        tn3270.SendPresentationSpace();

        tn3270.CopyPresentationSpace();

        tn3270.SetAction(243);

        tn3270.SetCursor(253);

        tn3270.SendPresentationSpace();

        tn3270.CopyPresentationSpace();

        tn3270.SetAction(125);

        tn3270.SetCursor(345);

        tn3270.CopyStringToField(12, "2");

        tn3270.SendPresentationSpace();

        tn3270.CopyPresentationSpace();

        tn3270.SetAction(125);

        tn3270.SetCursor(166);

        tn3270.CopyStringToField(3, "logoff");

        tn3270.SendPresentationSpace();

        tn3270.CopyPresentationSpace();

        tn3270.DisconnectPresentationSpace();

        String screens_buffer = tn3270.GetPresentationSpace(0);
        String user = tn3270.GetSingleValueByKeyword(screens_buffer, "USER", "=", 7);
        String owner = tn3270.GetSingleValueByKeyword(screens_buffer, "OWNER", "=", 9);
        String defaultgrp = tn3270.GetSingleValueByKeyword(screens_buffer, "DEFAULT-GROUP", "=", 9);

        System.out.println("user:" + user + " owner:" + owner + " defaultgrp:" + defaultgrp);

    }

    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
}
