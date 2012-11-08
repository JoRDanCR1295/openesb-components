/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.ne;

import java.net.InetAddress;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public interface AccessControlIf {
    /**
     * DOCUMENT ME!
     *
     * @param src DOCUMENT ME!
     * @param srcPort DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean allowIncomingUDP(InetAddress src, int srcPort);

    /**
     * DOCUMENT ME!
     *
     * @param src DOCUMENT ME!
     * @param srcPort DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean allowIncomingTCP(InetAddress src, int srcPort);

    /**
     * DOCUMENT ME!
     *
     * @param dest DOCUMENT ME!
     * @param destport DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean allowOutgoingUDP(InetAddress dest, int destport);

    /**
     * DOCUMENT ME!
     *
     * @param dest DOCUMENT ME!
     * @param destport DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean allowOutgoingTCP(InetAddress dest, int destport);
}
