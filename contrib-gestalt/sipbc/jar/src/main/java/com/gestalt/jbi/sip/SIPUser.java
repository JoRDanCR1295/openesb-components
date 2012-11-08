/**
 *   sip-binding-component - SIP Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com/
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.sip;

import com.gestalt.jbi.sip.component.SIPConfigExtensions;
import com.sun.jbi.internationalization.Messages;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ScheduledFuture;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.sip.Transaction;


/**
 * Identifies a single entity consuming the SIP BC. One SIPUser may have (and
 * most likely will) multiple SUs deployed to the SIP BC.
 *
 * @author : csturtz
 */
public final class SIPUser {
    private static final Logger log = Messages.getLogger(SIPUser.class);
    public static final int DEFAULT_TIMEOUT = 5000;
    private static final String AT = "@";
    private static Map<String, SIPUser> users = new HashMap<String, SIPUser>();
//    public static SIPConfigExtensions configExtentions;
    private String username;

    //private String password;
    private String proxyhost;
    private Integer proxyport;
    private Integer proxytimeout;
    private ScheduledFuture<?> scheduledFuture;
    private Integer activeEndpoints = 0;
    private Map<String, SIPSession> sessions = new HashMap<String, SIPSession>();

    /**
     * Private constructor only called by the getSIPUser() method. Simply sets
     * initial values.
     *
     * @param username
     * @param proxyhost
     * @param proxyport
     * @param proxytimeout
     */
    private SIPUser(String username, String proxyhost, Integer proxyport,
        Integer proxytimeout) {
        this.username = username;
        //this.password = password;
        this.proxyhost = proxyhost;
        this.proxyport = proxyport;

        if ((proxytimeout != null) && (proxytimeout > 0)) {
            this.proxytimeout = proxytimeout;
        } else {
            this.proxytimeout = DEFAULT_TIMEOUT;
        }
    }

    /**
     * We want to manage the instances of a SIPUser to ensure that there is only
     * one instance to a unique username and proxyhost combination.
     *
     * Makes use of Dependency Injection. Most all member variables should not
     * be reset after their inital values are provided. ** Note ** Maybe any
     * variables whose values may change after the instance is created should be
     * refactored to another class like SIPConnection.
     *
     * @param username
     * @param password
     * @param proxyhost
     * @param proxyport
     * @param proxytimeout
     * @return
     * @throws Exception
     */
    public static SIPUser getSIPUser(String username, String password,
        String proxyhost, Integer proxyport, Integer proxytimeout)
        throws Exception {
        String id = generateUniqueID(username, proxyhost);
        SIPUser user;

        if (users.containsKey(id)) {

            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER,"The SIPUser Already Exists. Retrieving the existing user.");
            }
            user = users.get(id);

            //todo if a wsdl is (re)deployed with a different password should it overwrite this one?.
        } else {

            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER,"Creating a new SIPUser");

            }
            user = new SIPUser(username, proxyhost, proxyport, proxytimeout);

                SIPConfigExtensions.getUsers().put(id, password);

            users.put(id, user);
        }

        return user;
    }

    public static SIPUser removeSIPUser(SIPUser user) {
        String key = SIPUser.generateUniqueID(user.getUsername(),
                user.getProxyhost());

        return users.remove(key);
    }

    public String getUsername() {
        return username;
    }

    public String getPassword() {
        return SIPConfigExtensions.getUsers()
                               .get(generateUniqueID(username, proxyhost));
    }

    public String getProxyhost() {
        return proxyhost;
    }

    public Integer getProxyport() {
        return proxyport;
    }

    public Integer getProxytimeout() {
        return proxytimeout;
    }

    public void setScheduledFuture(ScheduledFuture<?> future) {
        this.scheduledFuture = future;
    }

    public ScheduledFuture<?> getScheduledFuture() {
        return this.scheduledFuture;
    }

    public Integer getActiveEndpoints() {
        return this.activeEndpoints;
    }

    public void incrementActiveEndpoints() {
        this.activeEndpoints++;
    }

    public void decrementActiveEndpoints() {
        this.activeEndpoints--;
    }

    public SIPSession getSession(String remoteUri) {
        return sessions.get(remoteUri);
    }

    public void addSession(SIPSession session) throws Exception {
        if (sessions.get(session.getRemoteUri()) != null) {
            throw new Exception(
                "A session already established with the remoteUri or a session is being added");
        } else {
            sessions.put(session.getRemoteUri(), session);
        }
    }

    public void addSession(String remoteUri, Transaction tx)
        throws Exception {
        if (sessions.get(remoteUri) != null) {
            throw new Exception(
                "A session already established with the remoteUri or a session is being added");
        } else {
            SIPSession session = new SIPSession(remoteUri, tx);
            sessions.put(remoteUri, session);
        }
    }

    public SIPSession removeSession(String remoteUri) {
        return sessions.remove(remoteUri);
    }

    public boolean sessionExists(String remoteUri) {
        return (sessions.get(remoteUri) != null);
    }

    /**
     * Generates an ID that uniquely defines an instance of SIPUser. This allows
     * us to keep the one-to-one ration of SIPUSer instances to unique
     * combination of username + proxyhost.
     *
     * @param username
     * @param proxyhost
     * @return
     */
    public static String generateUniqueID(String username, String proxyhost) {
        StringBuilder sb = new StringBuilder();
        sb.append(username);
        sb.append(AT);
        sb.append(proxyhost);

        return sb.toString();
    }
}
