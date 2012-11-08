/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.ne;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.1 $
  */
public class AppEnv {
    private static String _nodeName = "/NVS-ES1";
    private static String _projectRoot = "/project";
    private static boolean _isServer = false;
    private static String _storageRoot = null;
    private static String _fsRootDir = "c:/nvs_data";

    //private static String _fsNetworkDir = _nodeName+"/network";
    private static String _fsNetworkDir = "/network";
    private static String _fsDeviceDir = _fsNetworkDir + "/devices";
    private static String _fsGroupDir = _fsNetworkDir + "/groups";
    private static int _numTftpsClientThreads = 10;

    //private static String _serverDir = _nodeName+"/server";
    private static String _serverDir = "/server";
    private static String _tempDir = _serverDir + "/temp";
    private static String _logsDir = _serverDir + "/logs";
    private static String _cfgArchiveDir = _serverDir + "/config-archive";
    private static String _cfgTemplateDir = _serverDir + "/config-template";
    private static String _imageDir = _serverDir + "/images";
    private static String _bootCfgFile = "bootconfig.txt";
    private static boolean _isAppliance = false;
    private static String _osName = null;
    private static boolean _isWindows = false;
    private static boolean _isLinux = false;
    private static int _miniHttpdPort = 8000;
    private static int _miniHttpdMaxThreads = 5;
    private static String _ssid = "nvs";
    private static String _deviceName = "AP";

    /**
     * DOCUMENT ME!
     */
    static void initAll() {
        _osName = System.getProperty("os.name");

        if (_osName.indexOf("Windows") >= 0) {
            _isWindows = true;
        } else {
            _isWindows = false;
        }

        if (_osName.indexOf("Linux") >= 0) {
            _isLinux = true;
        } else {
            _isLinux = false;
        }

        String p = System.getProperty("projectRoot");

        if (p != null) {
            _projectRoot = p;
        }

        p = System.getProperty("fsRootDir");

        if (p != null) {
            _fsRootDir = p;
        }

        p = System.getProperty("isAppliance");
        _isAppliance = ((p != null) && p.equalsIgnoreCase("true"));
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getProjectRoot() {
        return _projectRoot;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static boolean isServer() {
        return _isServer;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static boolean isAppliance() {
        return _isAppliance;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static boolean isWindows() {
        return _isWindows;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static boolean isLinux() {
        return _isLinux;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getResourcesRoot() {
        if (_isServer) {
            return _projectRoot + "/resources/server";
        } else {
            return _projectRoot + "/resources/client";
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getMiniHttpdDocRoot() {
        return getResourcesRoot() + "/httpd/htdocs/";
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static int getMiniHttpdPort() {
        return _miniHttpdPort;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static int getMiniHttpdMaxThreads() {
        return _miniHttpdMaxThreads;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getAlarmsResourcesDir() {
        return getResourcesRoot() + "/alarms";
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getConfigResourcesDir() {
        return getResourcesRoot() + "/config";
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getStorageRoot() {
        if (_storageRoot != null) {
            return _storageRoot;
        }

        return _projectRoot + "/storage";
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getFsRootDir() {
        return _fsRootDir;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getFsNetworkDir() {
        return _fsNetworkDir;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getFsDeviceDir() {
        return _fsDeviceDir;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getFsGroupDir() {
        return _fsGroupDir;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getTempDir() {
        return _tempDir;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getServerDir() {
        return _serverDir;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getBootConfigFile() {
        return _bootCfgFile;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getConfigArchiveDir() {
        return _cfgArchiveDir;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getUserTemplateDir() {
        return _cfgTemplateDir;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getImageDir() {
        return _imageDir;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getDefaultSsid() {
        return _ssid;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getDefaultDeviceName() {
        return _deviceName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static int getNumTftpsClientThreads() {
        return _numTftpsClientThreads;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getNodeName() {
        return _nodeName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getLogsDir() {
        return _logsDir;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getIANAEnterpiseNumbersFile() {
        return getResourcesRoot() + "/mibs/enterprise-numbers.txt";
    }

    /**
     * DOCUMENT ME!
     */
    public static void initServerEnv() {
        _isServer = true;

        initAll();
    }
}
