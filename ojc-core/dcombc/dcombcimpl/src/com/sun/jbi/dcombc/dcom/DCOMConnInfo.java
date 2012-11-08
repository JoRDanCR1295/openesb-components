/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/
package com.sun.jbi.dcombc.dcom;

public class DCOMConnInfo {
    /**
     *
     */
    private String mInterface;
    private String mMethod;
    private String mDomain;
    private String mServer;
    private String mUserName;
    private String mPassword;

    /**
     * @return the Interface
     */
    public String getInterface() {
        return mInterface;
    }

    /**
     * @param interface1 the mInterface to set
     */
    public void setInterface(String interface1) {
        mInterface = interface1;
    }

    /**
     * @return the Method
     */
    public String getMethod() {
        return mMethod;
    }

    /**
     * @param method the Method Name to set
     */
    public void setMethod(String method) {
        mMethod = method;
    }

    /**
     * @return the Domain
     */
    public String getDomain() {
        return mDomain;
    }

    /**
     * @param domain the mDomain to set
     */
    public void setDomain(String domain) {
        mDomain = domain;
    }

    /**
     * @return the Server
     */
    public String getServer() {
        return mServer;
    }

    /**
     * @param server the Server to set
     */
    public void setServer(String server) {
        mServer = server;
    }

    /**
     * @return the UserName
     */
    public String getUserName() {
        return mUserName;
    }

    /**
     * @param userName the UserName to set
     */
    public void setUserName(String userName) {
        mUserName = userName;
    }

    /**
     * @return the Password
     */
    public String getPassword() {
        return mPassword;
    }

    /**
     * @param password the Password to set
     */
    public void setPassword(String password) {
        mPassword = password;
    }
}
