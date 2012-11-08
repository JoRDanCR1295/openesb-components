/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.tst.svc;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

/**
 *
 * @author gpatil
 */
public class Counter {
    public static final String TestDeleteStr = "TestDelete" ; //NOI18N
    public static final String TestErrorStr = "TestError" ;   //NOI18N
    public static final String TestRedirectStr = "TestRedirect" ;   //NOI18N
    public static final String TestErrorInOutStr = "TestErrorInOut" ;   //NOI18N
    public static final String TestRedirectInOutStr = "TestRedirectInOut" ;   //NOI18N
    public static final String TestBPELErrorStr = "TestBPELError" ;   //NOI18N
    public static final String TestRedirectFromBPELStr = "TestRedirectFromBPEL" ; //NOI18N

    public static final AtomicInteger TestDeleteCounter = new AtomicInteger();
    public static final AtomicInteger TestErrorCounter = new AtomicInteger();
    public static final AtomicInteger TestRedirectCounter = new AtomicInteger();
    public static final AtomicBoolean CalledRedirectedSvc = new AtomicBoolean(false);
    public static final AtomicBoolean CalledRedirectedInOutSvc = new AtomicBoolean(false);
}
