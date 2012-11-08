/*
 * @(#)file      LinkTrapGeneratorMBean.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.25
 * @(#)lastedit  04/04/07
 *
 * Copyright 2004 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.soabi.snmpagent;

/**
 * This interface exposes the remote management interface of the
 * "LinkTrapGenerator" MBean.
 */

public interface LinkTrapGeneratorMBean {
    
    public Integer getIfIndex() ;

    public void setIfIndex(Integer x) ;
    
    public Integer getSuccesses() ;

    public Integer getErrors() ;

    public Integer getInterval() ;

    public void setInterval(Integer val) ;
}

