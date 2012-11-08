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
public final class DeviceCapability {
    /**
     * DOCUMENT ME!
     */
    public static final Enumerator enumerator = new Enumerator(DeviceCapability.class);

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_UNKNOWN_TYPE = 0;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_UNKNOWN_TYPE_label = "unknown-device-type";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_NVS_SERVER = 1;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_NVS_SERVER_label = "sunnvs-server";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_NETWORK_MANAGER = 2;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_NETWORK_MANAGER_label = "nms-platform";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_BRIDGE = 10;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_BRIDGE_label = "generic-bridge";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_ROUTER = 11;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_ROUTER_label = "l3-router";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_SWITCH = 12;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_SWITCH_label = "l2-switch";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_FIREWALL = 13;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_FIREWALL_label = "firewall";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_VPN = 14;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_VPN_label = "vpn";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_IDS = 15;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_IDS_label = "ids";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_HOST = 20;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_HOST_label = "generic-host";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_DOT11_AP = 100;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_DOT11_AP_label = "dot11-ap";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_DOT11_INFRASTRUCTURE_CLIENT = 101;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_DOT11_INFRASTRUCTURE_CLIENT_label = "dot11-infrastructure-client-mode";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_DOT11B = 111;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_DOT11B_label = "dot11b-radio";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_DOT11A = 112;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_DOT11A_label = "dot11a-radio";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_DOT11G = 113;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_DOT11G_label = "dot11g-radio";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_IOS = 300;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_IOS_label = "os-type-ios";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_CATOS = 301;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_CATOS_label = "os-type-catos";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_PIXOS = 302;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_PIXOS_label = "os-type-pixos";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_IOS_STYLE_CLI = 500;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_IOS_STYLE_CLI_label = "ios-style-commands";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_SYSLOG_GENERATION = 1000;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_SYSLOG_GENERATION_label = "sylog-source";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_RF_MONITOR_MODE = 1100;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_RF_MONITOR_MODE_label = "radio-monitor-mode";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_TFTP_CLIENT = 5000;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_TFTP_CLIENT_label = "tftp-client";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_TFTP_SERVER = 5002;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_TFTP_SERVER_label = "tftp-server";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_CDP_MIB_SUPPORTED = 16000;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_CDP_MIB_SUPPORTED_label = "cdp-mib-supported";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_CDP_ENABLED = 16001;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_CDP_ENABLED_label = "cdp-enabled";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_FLASH_MIB_SUPPORTED = 16002;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_FLASH_MIB_SUPPORTED_label = "flash-mib-supported";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_VTP_MIB_SUPPORTED = 16003;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_VTP_MIB_SUPPORTED_label = "vtp-mib-supported";

    /**
     * DOCUMENT ME!
     */
    public static final int CAPABILITY_VTP_SUPPORTED = 16004;

    /**
     * DOCUMENT ME!
     */
    public static final String CAPABILITY_VTP_SUPPORTED_label = "vtp-supported";
    private int[] capFlags;

    /**
     * Creates a new DeviceCapability object.
     *
     * @param flags DOCUMENT ME!
     */
    public DeviceCapability(int[] flags) {
        capFlags = flags;
    }

    /**
     * DOCUMENT ME!
     *
     * @param flag DOCUMENT ME!
     */
    public synchronized void addCapability(int flag) {
        if (hasCapability(flag)) {
            return;
        }

        int[] arr = new int[capFlags.length + 1];
        System.arraycopy(capFlags, 0, arr, 0, capFlags.length);
        arr[arr.length - 1] = flag;

        capFlags = arr;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public synchronized int[] getFlags() {
        return capFlags;
    }

    /**
     * DOCUMENT ME!
     *
     * @param cap DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public synchronized boolean hasCapability(String cap) {
        int flag = enumerator.toValue(cap);

        return hasCapability(flag);
    }

    /**
     * DOCUMENT ME!
     *
     * @param flag DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public synchronized boolean hasCapability(int flag) {
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
    public synchronized String toCompactString() {
        StringBuffer sb = new StringBuffer();

        for (int i = 0; i < capFlags.length; i++) {
            if (i != 0) {
                sb.append(", ");
            }

            sb.append(enumerator.toLabel(capFlags[i]));
        }

        return sb.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public synchronized String toString() {
        StringBuffer sb = new StringBuffer();

        for (int i = 0; i < capFlags.length; i++) {
            sb.append("\t" + enumerator.toLabel(capFlags[i]) + ", \n");
        }

        return sb.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public synchronized String toXMLString() {
        StringBuffer sb = new StringBuffer();

        String[] caps = enumerator.getAllLabels();

        sb.append("<Capabilities>");

        for (int i = 0; i < caps.length; i++) {
            sb.append("<Capability>" + caps[i] + "</Capability>");
        }

        sb.append("</Capabilities>");

        return sb.toString();
    }
}
