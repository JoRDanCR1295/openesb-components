/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.ne;

import com.sun.nvs.core.util.Enumerator;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public final class InterfaceCapability {
    /**
     * DOCUMENT ME!
     */
    public static final Enumerator enumerator = new Enumerator(InterfaceCapability.class);

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_UNKNOWN_TYPE = 0;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_UNKNOWN_TYPE_label = "type-unknown";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_OTHER = 1;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_OTHER_label = "type-other";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_PHYSICAL = 2;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_PHYSICAL_label = "physical-interface";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_VIRTUAL = 3;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_VIRTUAL_label = "virtual-interface";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_LOOPBACK = 4;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_LOOPBACK_label = "type-loopback";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_SUB_INTERFACE = 5;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_SUB_INTERFACE_label = "sub-interface";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_ETHERNET = 10;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_ETHERNET_label = "type-ethernet";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_FAST_ETHERNET = 11;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_FAST_ETHERNET_label = "type-fastethernet";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_GIG_ETHERNET = 12;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_GIG_ETHERNET_label = "type-gigethernet";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_VLAN = 13;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_VLAN_label = "type-vlan";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_SERIAL = 14;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_SERIAL_label = "type-serial";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_BRI = 15;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_BRI_label = "type-bri";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_AUI = 16;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_AUI_label = "type-aui";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_PORTCHANNEL = 17;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_PORTCHANNEL_label = "type-portchannel";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_SHORTWAVEFIBER = 18;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_SHORTWAVEFIBER_label = "type-shortwave-fiber";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_MULTIMODEFIBER = 19;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_MULTIMODEFIBER_label = "type-multimode-fiber";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_DOT11B = 110;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_DOT11B_label = "type-dot11b";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_DOT11 = 111;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_DOT11_label = "type-any-dot11";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_DOT11A = 112;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_DOT11A_label = "type-dot11a";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_DOT11G = 113;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_DOT11G_label = "type-dot11g";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_MANAGEMENT = 150;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_MANAGEMENT_label = "management-interface";
    private int[] capFlags;

    /**
     * Creates a new InterfaceCapability object.
     *
     * @param flags DOCUMENT ME!
     */
    public InterfaceCapability(int[] flags) {
        capFlags = flags;
    }

    /**
     * Creates a new InterfaceCapability object.
     *
     * @param flag DOCUMENT ME!
     */
    public InterfaceCapability(int flag) {
        capFlags = new int[1];
        capFlags[0] = flag;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int[] getFlags() {
        return capFlags;
    }

    /**
     * DOCUMENT ME!
     *
     * @param flag DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean hasCapability(int flag) {
        for (int i = 0; i < capFlags.length; i++)
            if (capFlags[i] == flag) {
                return true;
            }

        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toString() {
        StringBuffer sb = new StringBuffer();

        for (int i = 0; i < capFlags.length; i++) {
            sb.append("\t" + enumerator.toLabel(capFlags[i]) + "\n");
        }

        return sb.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @param hint DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static int getCapability(String hint) {
        return (enumerator.toFlag(hint));
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static int[] getAllCapabilities() {
        return enumerator.getAllLValues();
    }
}
