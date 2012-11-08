/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.ne;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public class TermServerInfo {
    /**
     * DOCUMENT ME!
     */
    public static final int PREFERENCE_PRIMARY = 1;

    /**
     * DOCUMENT ME!
     */
    public static final int PREFERENCE_FALLBACK = 2;
    private IpAddress _address;
    private int _port = 23;
    private int _preference = PREFERENCE_FALLBACK;

    /**
     * Creates a new TermServerInfo object.
     */
    public TermServerInfo() {
    }

    /**
     * DOCUMENT ME!
     *
     * @param ip DOCUMENT ME!
     */
    public void setTerminalServerAddress(IpAddress ip) {
        _address = ip;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public IpAddress getTerminalServerAddress() {
        return _address;
    }

    /**
     * DOCUMENT ME!
     *
     * @param port DOCUMENT ME!
     */
    public void setTerminalServerPort(int port) {
        _port = port;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getTerminalServerPort() {
        return _port;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getPreference() {
        return _preference;
    }

    /**
     * DOCUMENT ME!
     */
    public void setAsPrimaryTransport() {
        _preference = PREFERENCE_PRIMARY;
    }

    /**
     * DOCUMENT ME!
     */
    public void setAsFallbackTransport() {
        _preference = PREFERENCE_FALLBACK;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isPrimaryTransport() {
        return (_preference == PREFERENCE_PRIMARY);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toXMLString() {
        StringBuffer sb = new StringBuffer();
        sb.append("<TermServerInfo>");
        sb.append("<IPAddress>" + _address + "</IPAddress>");
        sb.append("<Port>" + _port + "</Port>");

        if (_preference == PREFERENCE_FALLBACK) {
            sb.append("<Preference>fallback</Preference>");
        } else {
            sb.append("<Preference>primary</Preference>");
        }

        sb.append("</TermServerInfo>");

        return sb.toString();
    }
}
