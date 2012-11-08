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
public interface CredentialSet {
    /**
     * DOCUMENT ME!
     */
    public static final int TRANSPORT_TYPE_UNSET = -1;

    /**
     * DOCUMENT ME!
     */
    public static final int TRANSPORT_TYPE_UNKNOWN = 0;

    /**
     * DOCUMENT ME!
     */
    public static final int TRANSPORT_TYPE_TELNET = 1;

    /**
     * DOCUMENT ME!
     */
    public static final int TRANSPORT_TYPE_SSH = 2;

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getSetName();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getReadCommunity();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getWriteCommunity();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getLogin();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getPassword();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getEnablePassword();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean hasChanged();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getTransportType();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public CipherSuite getSshCipherSuite();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getWeightage();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public TermServerInfo getTerminalServerInfo();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toXMLString();
}
