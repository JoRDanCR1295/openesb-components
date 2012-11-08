package com.sun.jbi.common.qos.throttling;

/**
 *
 * @author VFrolkin
 */
public interface ThrottlingLeakyConfig extends ThrottlingConfig {

    //<th:throttling leakRate="10" />
    public int getLeakRate();
}
