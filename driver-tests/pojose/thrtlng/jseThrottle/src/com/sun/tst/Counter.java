/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.tst;

import java.util.concurrent.atomic.AtomicInteger;

/**
 *
 * @author gpatil
 */
public class Counter {
    public static final String TestSerialThrottleStr = "TestSerialThrottle" ; //NOI18N
    public static final String TestThrottleStr = "TestThrottle" ; //NOI18N

    public static final AtomicInteger CallRcvd = new AtomicInteger();
    public static final AtomicInteger MaxConcurrentCallRcvd = new AtomicInteger();
    public static final AtomicInteger ConcurrentCallInProcess = new AtomicInteger();
}
