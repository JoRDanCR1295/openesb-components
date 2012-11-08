/*
 * BEGIN_HEADER - DO NOT EDIT
 *
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)JBIComponentsInstaller.java
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.installer;

import com.sun.appserv.addons.Installer;
import com.sun.appserv.addons.InstallationContext;
import com.sun.appserv.addons.AddonException;
import com.sun.appserv.addons.AddonVersion;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.logging.Logger;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.Date;
import java.util.zip.ZipException;

/**
 * This class is the main entry point to the installer for
 * JBI Addons Lite.  
 */
public class JBIComponentsInstaller implements Installer {

    private String mAntScriptName = null;
    private String mAntBaseDir = null;
    private Logger mLogger = null;
    private Properties mCmdLineParams = new Properties();
    private boolean mIsJBIAddonsInstalled = true; 
    private boolean mIsJBIUninstalled = true; 
    private ResourceBundle resourceBundle = null;

    /**
     * The main method of the JBI components Addons installer.  
     * This method takes the appserver installation dir as the
     * first argument
     * @param args, the first argument should be appserver install dir
     *
     */
    public static void main(String[] args)
    {
        JBIComponentsInstaller jbiAddonsInstaller = null;
        try
        {
            boolean isInstall = true;
            jbiAddonsInstaller = new JBIComponentsInstaller();
            jbiAddonsInstaller.setResourceBundle();
            jbiAddonsInstaller.setLogger();

            if (args.length <1 || args[0] == null || args[0].equals("") ) {
                jbiAddonsInstaller.getLogger().severe(
                             jbiAddonsInstaller.getResourceBundle().getString("usage"));
                return;
            }


            //If secoond argument is not given default to install
            //this second argument is used to decide to install or uninstall
            if (args.length <2 || args[1] == null || args[1].equalsIgnoreCase("install")) {
                isInstall = true;
            } else {
                isInstall = false;
            }

            InstallationContext ic = new InstallationContext();
            File asDir = new File(args[0]);
            ic.setInstallationDirectory(asDir);
            
            if (isInstall) {
                jbiAddonsInstaller.install(ic);
                if (jbiAddonsInstaller.mIsJBIAddonsInstalled){
                    jbiAddonsInstaller.getLogger().info(
                        jbiAddonsInstaller.getResourceBundle().getString(
                                    "installation-successful") + args[0] + File.separator +
                                    InstallConstants.JBI_INSTALL_DIR);
                } else {
                    jbiAddonsInstaller.getLogger().severe(
                        jbiAddonsInstaller.getResourceBundle().getString(
                        "installation-failed"));
                }
            } else {
                jbiAddonsInstaller.uninstall(ic);
                if (jbiAddonsInstaller.mIsJBIUninstalled){
                    jbiAddonsInstaller.getLogger().info(
                        jbiAddonsInstaller.getResourceBundle().getString(
                                    "uninstallation-successful") + args[0] + File.separator +
                                    InstallConstants.JBI_INSTALL_DIR);
                } else {
                    jbiAddonsInstaller.getLogger().severe(
                        jbiAddonsInstaller.getResourceBundle().getString(
                        "uninstallation-failed"));
                }
            }

        } catch(Exception e) {
            jbiAddonsInstaller.getLogger().severe(e.getMessage());
        }

    }

    /**
     * This method is used to install JBI components
     * @param InstallationContext ic
     */
    public void install(InstallationContext ic) throws AddonException
    {
        String timeStamp = "";
        try {
            String pattern = "yyyyMMddHHmm";
            String underscore = "_";
            SimpleDateFormat df = new SimpleDateFormat(pattern);
            String result = df.format(new Date());
            timeStamp = underscore + result;
        } catch (Exception e) {
        //Here we do not do anything and the timestamp will be an empty string
        }

        mLogger = Logger.getLogger("com.sun.jbi.installer.JBIComponentsInstaller");
        setResourceBundle();

        mAntBaseDir = System.getProperty("java.io.tmpdir") +
                          File.separator + "jbi-components" + timeStamp;

        mAntScriptName = mAntBaseDir + File.separator +
                            InstallConstants.JBI_INSTALL_SCRIPT;
                            
        File appserverDir = ic.getInstallationDirectory();
        String appServerInstallRoot = appserverDir.getAbsolutePath();

        String jbiComponentsJar =
                            appServerInstallRoot + File.separator +
                            "addons" + File.separator +
                            InstallConstants.JBI_COMPONENTS_INSTALLER_JAR;

        File baseDir = new File(mAntBaseDir);
        if(!baseDir.exists()) {
           baseDir.mkdirs();
        }

        try {
            JarFactory jrFctry = new JarFactory(mAntBaseDir);
            jrFctry.unJar(new File(jbiComponentsJar));
        } catch (ZipException zipEx) {
            mIsJBIAddonsInstalled = false;
            mLogger.severe(getResourceBundle().getString("jar-exception") + jbiComponentsJar +  zipEx.getMessage());
            throw new AddonException(getResourceBundle().getString("jar-exception") + jbiComponentsJar +  zipEx.getMessage());
        } catch (IOException ioEx) {
            mIsJBIAddonsInstalled = false;
            mLogger.severe(getResourceBundle().getString("copy-jar-to-addons-dir") + jbiComponentsJar + ioEx.getMessage());
            throw new AddonException(getResourceBundle().getString("copy-jar-to-addons-dir") + jbiComponentsJar + ioEx.getMessage());
        }

        // if AS_INSTALL for windows contains \, replace with / and
        // set them in build.properties file
        if (appServerInstallRoot.indexOf("\\") != -1) {
              appServerInstallRoot = appServerInstallRoot.replace('\\', '/');
        }
        
        mCmdLineParams.setProperty(
            InstallConstants.AS_INSTALL_DIR, appServerInstallRoot);


        //the log entries in tmp dir and then move it to jbi dir
        String tempLogFile = mAntBaseDir + File.separator +
                             "jbi-components.log";

        try {
                AntRunner ant = new AntRunner();
            if (!ant.runAnt(appServerInstallRoot, mAntScriptName,
                InstallConstants.JBI_INSTALL_TARGET, mCmdLineParams, tempLogFile))
            {
                mLogger.severe(getResourceBundle().getString(
                        "ant-script-failed") + mAntScriptName);
                throw new AddonException(getResourceBundle().getString(
                       "ant-script-failed") );
            }
        } catch (Exception e) {
            mIsJBIAddonsInstalled = false;
            mLogger.severe(getResourceBundle().getString(
                       "Unable-to-execute-ant-script") + e.getMessage());
            throw new AddonException(getResourceBundle().getString(
                       "Unable-to-execute-ant-script") + e.getMessage());
        }

        String logFile = appServerInstallRoot + File.separator +
                    InstallConstants.JBI_INSTALL_DIR + File.separator +
                    "jbi-components.log";
        try {
            InstallerUtilities.copyFile(new File(tempLogFile), new File(logFile));
        } catch (Exception e) {
             mLogger.warning(getResourceBundle().getString(
                       "error-in-moving-log-file") + logFile +  e.getMessage());
            throw new AddonException(getResourceBundle().getString(
                       "error-in-moving-log-file") + e.getMessage());
        }
            boolean deleted = deleteDirAndContents(baseDir, true);
    }

    /**
     * This method is used to uninstall JBI Addons Lite
     * @param InstallationContext ic
     */
    public void uninstall(InstallationContext ic) throws AddonException
    {
        mLogger = Logger.getLogger("com.sun.jbi.installer.JBIComponentsInstaller");
        setResourceBundle();

        try {       
            File appserverDir = ic.getInstallationDirectory();
            String appServerInstallRoot = appserverDir.getAbsolutePath();
            
            String utilJarStr = appServerInstallRoot + File.separator + "lib" + 
                            File.separator + "addons" + File.separator + InstallConstants.JBI_INSTALL_UTIL_JAR;
            File utilJar = new File(utilJarStr);
            utilJar.delete();
            
            String configJarStr = appServerInstallRoot + File.separator + "lib" + 
                            File.separator + "addons" + File.separator + InstallConstants.JBI_COMPONENTS_CONFIGURATOR_JAR;
            File configJar = new File(configJarStr);
            configJar.delete();
            
            String uninstallLogStr = appServerInstallRoot + File.separator + "addons" + File.separator + "jbi-components.log";
            File uninstallLog = new File(uninstallLogStr);
            uninstallLog.delete();


            String uninstallJarStr = appServerInstallRoot + File.separator + "addons" + File.separator + "jbi_components_installer.jar";
            File uninstallJar = new File(uninstallJarStr);
            uninstallJar.delete();


            String uninstallDirStr = appServerInstallRoot + File.separator + "addons" + File.separator + "jbi-components";
            File uninstallDir = new File(uninstallDirStr);
            boolean deleted = deleteDirAndContents(uninstallDir, true);
                         
            mLogger.info(
                getResourceBundle().getString(
                "uninstallation-successful") + appServerInstallRoot + 
                File.separator +
                 InstallConstants.JBI_INSTALL_DIR);
        
        } catch (Exception e) {
            mIsJBIUninstalled = false;
            mLogger.severe(getResourceBundle().getString(
                       "error-in-uninstall-components") + e.getMessage());
            throw new AddonException(getResourceBundle().getString(
                       "error-in-uninstall-components") + e.getMessage());
        }
    }
    
    /**
     * This method is used during an upgrade.
     */
    public void upgrade (InstallationContext ic, AddonVersion ver)
    {
    }

    private boolean deleteDirAndContents(File path, boolean recursive) {
        if(path.exists()) {
            File[] files = path.listFiles();
            for(int i=0; i<files.length; i++) {
                if(files[i].isDirectory()) {
                    if (recursive) {
                        deleteDirAndContents(files[i], recursive);
                    }
                } else {
                    boolean removed = files[i].delete();
                          if (!removed) {
                                files[i].deleteOnExit();
                            }
                }
            }
        }
        boolean deleted = path.delete();
          if (!deleted) {
                path.deleteOnExit();
          }
          return true;
    }

  /**
   * This method is used to set Logger
   */
  private void setLogger()
  {
     mLogger = Logger.getLogger("com.sun.jbi.installer.JBIComponentsInstaller");
  }

  /**
   * This method is used to get Logger
   */
  private Logger getLogger()
  {
     return mLogger;
  }

  /**
   * This method is used to set ResourceBundle
   */
  private void setResourceBundle(){
      resourceBundle =
              ResourceBundle.getBundle(InstallConstants.RESOURCE_BUNDLE);
  }

  /**
   * This method is used to get ResourceBundle
   */
  public ResourceBundle getResourceBundle(){
      return resourceBundle;
  }

}
