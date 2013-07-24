/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.openesb.components.camelse;

import com.sun.jbi.common.qos.config.ComponentConfig;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author chikkala
 */
public interface CamelSEConfigMBean {

    public void setCamelHome(String camelHome);

    public String getCamelHome();
    
    public void setIncludeCamelLibs(String includeLibs);

    public String getIncludeCamelLibs();
    
    public void setExcludeCamelLibs(String excludeLibs);

    public String getExcludeCamelLibs();

    public static class CamelSEConfigMBeanImpl implements CamelSEConfigMBean {

        /** relative path in both installPath and workPath*/
        private static final String CONFIG_FILE_PATH = "config.properties";
        //private static final String PROP_CAMEL_HOME = "camel.home";
        private static final String PROP_CAMEL_HOME = "CamelHome";
        //private static final String PROP_EXCLUDE_CAMEL_LIBS = "exclude.camel.libs";
        private static final String PROP_EXCLUDE_CAMEL_LIBS = "ExcludeCamelLibs";
        //private static final String PROP_INCLUDE_CAMEL_LIBS = "include.camel.libs";
        private static final String PROP_INCLUDE_CAMEL_LIBS = "IncludeCamelLibs";
        
        
        private File mConfigFile;
        private Properties mConfigProps;

        public CamelSEConfigMBeanImpl(String installPath) {
            this(installPath, null);
        }

        /**
         * if workPath is null, properties are loaded
         * @param installPath
         * @param workPath
         */
        public CamelSEConfigMBeanImpl(String installPath, String workPath) {
            this.mConfigProps = new Properties();
            if (workPath == null) {
                this.mConfigFile = createConfigFile(installPath);
                loadProperties(this.mConfigProps, this.mConfigFile);
            } else {
                this.mConfigFile = createConfigFile(workPath);
                Properties installProps = new Properties();
                File installFile = findConfigFile(installPath);
                if (installFile != null) {
                    loadProperties(installProps, installFile);
                }
                this.mConfigProps.putAll(installProps);
                loadProperties(this.mConfigProps, this.mConfigFile);
            }
        }

        public void setInitialConfigurations(ComponentConfig props) {
            setCamelHome(props.getProperty(PROP_CAMEL_HOME).getValue());
            setIncludeCamelLibs(props.getProperty(PROP_INCLUDE_CAMEL_LIBS).getValue());
            setExcludeCamelLibs(props.getProperty(PROP_EXCLUDE_CAMEL_LIBS).getValue());
        }
        
        private File createConfigFile(String configRootPath) {
            File confFile = new File(configRootPath, CONFIG_FILE_PATH);
            if (!confFile.exists()) {
                try {
                    File confParent = confFile.getParentFile();
                    confParent.mkdirs();
                    FileWriter writer = new FileWriter(confFile);
                    writer.append("# CamelSe Configuration parameters");
                    writer.close();
                } catch (Exception ex) {
                    //TODO: log.
                    ex.printStackTrace();
                }
            }
            if (confFile.exists()) {
                return confFile;
            } else {
                return null;
            }
        }

        private File findConfigFile(String configRootPath) {
            File confFile = new File(configRootPath, CONFIG_FILE_PATH);
            if (confFile.exists()) {
                return confFile;
            } else {
                return null;
            }
        }

        private void saveProperties(File propFile, Properties props) {
            FileOutputStream outS = null;
            try {
                outS = new FileOutputStream(propFile);
                props.store(outS, "# CamelSe Configuration parameters");
            } catch (Exception ex) {
                ex.printStackTrace();
            } finally {
                if (outS != null) {
                    try {
                        outS.close();
                    } catch (IOException ex) {
                        Logger.getLogger(CamelSEConfigMBean.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }
            }
        }

        private void loadProperties(Properties props, File propFile) {

            FileInputStream inS = null;
            try {
                inS = new FileInputStream(propFile);
                props.load(inS);
            } catch (Exception ex) {
                ex.printStackTrace();
            } finally {
                if (inS != null) {
                    try {
                        inS.close();
                    } catch (IOException ex) {
                        Logger.getLogger(CamelSEConfigMBean.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }
            }
        }

        private StringBuffer readResourceAsString(String resourcePath) {
            
            StringWriter writer = new StringWriter();
            InputStream inS = null;
            try {
                inS = this.getClass().getResourceAsStream(resourcePath);
                InputStreamReader reader = new InputStreamReader(inS);
                char[] buff = new char[1024];
                int size = 0;
                while ((size = reader.read(buff)) != -1) {
                    writer.write(buff, 0, size);
                }
            } catch (Exception ex) {
                Logger.getLogger(CamelSEConfigMBean.class.getName()).log(Level.SEVERE, null, ex);
            } finally {
                if ( inS != null ) {
                    try {
                        inS.close();
                    } catch (IOException ex) {
                        Logger.getLogger(CamelSEConfigMBean.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }
            }
            return writer.getBuffer();
        }

        public void setCamelHome(String camelHome) {
            this.mConfigProps.setProperty(PROP_CAMEL_HOME, camelHome);
            this.saveProperties(mConfigFile, mConfigProps);
        }

        public String getCamelHome() {
            return this.mConfigProps.getProperty(PROP_CAMEL_HOME, "");
        }
        
        public void setIncludeCamelLibs(String includeLibs) {
            this.mConfigProps.setProperty(PROP_INCLUDE_CAMEL_LIBS, includeLibs);
            this.saveProperties(mConfigFile, mConfigProps);
        }

        public String getIncludeCamelLibs() {
            return this.mConfigProps.getProperty(PROP_INCLUDE_CAMEL_LIBS, "");
        }
        
        public void setExcludeCamelLibs(String excludeLibs) {
            this.mConfigProps.setProperty(PROP_EXCLUDE_CAMEL_LIBS, excludeLibs);
            this.saveProperties(mConfigFile, mConfigProps);
        }

        public String getExcludeCamelLibs() {
            return this.mConfigProps.getProperty(PROP_EXCLUDE_CAMEL_LIBS, "");
        }

    }
}
