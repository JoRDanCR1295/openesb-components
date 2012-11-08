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
 * @(#)ConnectionInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.connection;

import org.apache.mina.common.IoHandler;
import org.apache.mina.common.IoServiceConfig;

public class ConnectionInfo {

    private String host = "";

    private int port = 0;

    private String retryLogicString = "";

    private IoHandler ioHandler;

    private IoServiceConfig ioServiceConfig;

    private int minPoolSize = 2;

    private int maxPoolSize = 32;

    private long maxIdleTimeout = 60000;
    
    private String mEndpointName = "";

    public String getHost() {
        return host;
    }

    public void setHost(String host) {
        if (host != null) {
            this.host = host;
        }
    }

    public IoHandler getIoHandler() {
        return ioHandler;
    }

    public void setIoHandler(IoHandler ioHandler) {
        this.ioHandler = ioHandler;
    }

    public IoServiceConfig getIoServiceConfig() {
        return ioServiceConfig;
    }

    public void setIoServiceConfig(IoServiceConfig ioServiceConfig) {
        this.ioServiceConfig = ioServiceConfig;
    }

    public int getPort() {
        return port;
    }

    public void setPort(int port) {
        this.port = port;
    }

    public String getRetryLogicString() {
        return retryLogicString;
    }

    public void setRetryLogicString(String retryLogicString) {
        if (retryLogicString != null) {
            this.retryLogicString = retryLogicString;
        }
    }

    public boolean equals(Object obj) {
        if (obj != null && obj instanceof ConnectionInfo) {
            ConnectionInfo connInfo = (ConnectionInfo) obj;
            return (connInfo.getHost().equals(this.getHost()) &&
            // connInfo.getPort() == this.getPort()&& connInfo.getIoHandler() ==
            // this.getIoHandler());
            connInfo.getPort() == this.getPort() && connInfo.getEndpointName().equals(this.getEndpointName()));
        } else {
            return false;
        }
    }

    public int hashCode() {
        return this.host.hashCode() + this.port + this.mEndpointName.hashCode();
    }

    public String toString() {
        return this.host + ":" + this.port;
    }

    public int getMinPoolSize() {
        return minPoolSize;
    }

    public void setMinPoolSize(int minPoolSize) {
        this.minPoolSize = minPoolSize;
    }

    public int getMaxPoolSize() {
        return maxPoolSize;
    }

    public void setMaxPoolSize(int maxPoolSize) {
        this.maxPoolSize = maxPoolSize;
    }

    public long getMaxIdleTimeout() {
        return maxIdleTimeout;
    }

    public void setMaxIdleTimeout(long maxIdleTimeout) {
        this.maxIdleTimeout = maxIdleTimeout;
    }
    

    public String getEndpointName() {
        return mEndpointName;
    }

    public void setEndpointName(String endpointName) {
        this.mEndpointName = endpointName;
    }

}
