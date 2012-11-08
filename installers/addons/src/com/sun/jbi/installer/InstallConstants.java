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
 * @(#)InstallConstants.java
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.installer;

import java.io.File;
/**
 *
 * This class stores all the constants required
 */
public class InstallConstants {

    public static final String JBI_COMPONENTS_INSTALLER_JAR = "jbi_components_installer.jar";
    public static final String JBI_ADDITIONAL_COMPONENTS_INSTALLER_JAR = "jbi_additional_components_installer.jar";
    public static final String JBI_INSTALL_SCRIPT = "build.xml";
    public static final String JBI_INSTALL_TARGET = "install_jbi";

	public static final String JBI_INSTALL_UTIL_JAR = "install-util.jar";
    public static final String JBI_COMPONENTS_CONFIGURATOR_JAR = "jbi_components_configurator.jar";
    public static final String JBI_ADDITIONAL_COMPONENTS_CONFIGURATOR_JAR = "jbi_additional_components_configurator.jar";
    public static final String JBI_CONFIGURE_SCRIPT = "build.xml";
    public static final String JBI_CONFIGURE_TARGET = "configure-components";
    public static final String JBI_FILE_NAME = "META-INF/jbi.xml"; // in the component jar file
    
    public static final String IS_DAS = "isDAS";
    
    public static final String BPELSE = "bpel";
    public static final String XSLTSE = "xslt";
    public static final String IEPSE = "iep";
    public static final String FILEBC = "file";
    public static final String JMSBC = "jms";
    public static final String JDBCBC = "jdbc";
    public static final String SMTPBC = "smtp";
    public static final String FTPBC = "ftp";
    public static final String MQBC = "webspheremq";
    public static final String SQLSE = "sql";
    public static final String ENCODER = "encoder";
    public static final String ENCODERLIB = "encoderlib";
    public static final String SAXON= "saxon";
    public static final String SAXONLIB = "saxonlib";
    public static final String WSDLEXT = "wsdl-ext";
    public static final String WSDLEXTLIB = "wsdlextlib";
    
    public static final String BPELSE_NAME = "bpelse.name";
    public static final String XSLTSE_NAME = "xsltse.name";
    public static final String IEPSE_NAME = "iepse.name";
    public static final String JMSBC_NAME = "jmsbc.name";
    public static final String FILEBC_NAME = "filebc.name";
    public static final String JDBCBC_NAME = "jdbcbc.name";
    public static final String SMTPBC_NAME = "smtpbc.name";
    public static final String FTPBC_NAME = "ftpbc.name";
    public static final String MQBC_NAME = "mqbc.name";
    public static final String SQLSE_NAME = "sqlse.name";
    public static final String ENCODERLIB_NAME = "encoderlib.name";
    public static final String SAXONLIB_NAME = "saxonlib.name";
    public static final String WSDLEXTLIB_NAME = "wsdlextlib.name";
    
    public static final String BPELSE_JAR = "bpelserviceengine.jar";
    public static final String XSLTSE_JAR = "xsltserviceengine.jar";
    public static final String IEPSE_JAR = "iepserviceengine.jar";
    public static final String FILEBC_JAR = "filebc.jar";
    public static final String JMSBC_JAR = "jmsbc.jar";
    public static final String JDBCBC_JAR = "jdbcbc.jar";
    public static final String SMTPBC_JAR = "smtpbc.jar";
    public static final String FTPBC_JAR = "ftpbc.jar";
    public static final String MQBC_JAR = "mqbc.jar";
    public static final String SQLSE_JAR = "sqlse.jar";
    public static final String ENCODERLIB_JAR = "encoderlib.jar";
    public static final String SAXONLIB_JAR = "saxonlib.jar";
    public static final String WSDLEXTLIB_JAR = "wsdlextlib.jar";
    
    public static final String BPELSE_JAR_NAME = "bpelse.jar.name";
    public static final String XSLTSE_JAR_NAME = "xsltse.jar.name";
    public static final String IEPSE_JAR_NAME = "iepse.jar.name";
    public static final String FILEBC_JAR_NAME = "filebc.jar.name";
    public static final String JMSBC_JAR_NAME = "jmsbc.jar.name";
    public static final String JDBCBC_JAR_NAME = "jdbcbc.jar.name";
    public static final String SMTPBC_JAR_NAME = "smtpbc.jar.name";
    public static final String FTPBC_JAR_NAME = "ftpbc.jar.name";
    public static final String MQBC_JAR_NAME = "mqbc.jar.name";
    public static final String SQLSE_JAR_NAME = "sqlse.jar.name";
    public static final String ENCODERLIB_JAR_NAME = "encoderlib.jar.name";
    public static final String SAXONLIB_JAR_NAME = "saxonlib.jar.name";
    public static final String WSDLEXTLIB_JAR_NAME = "wsdlextlib.jar.name";

    public static final String AS_INSTALL_DIR = "appserver.install.dir";
    public static final String AS_JDK_DIR = "appserver.jdk.dir";
    public static final String AS_DOMAIN_ROOT = "appserver.jbi.domain.root";
    public static final String RESOURCE_BUNDLE =
            "com.sun.jbi.installer.InstallerMessages";


    //the following two entries are relative to appserver install dir
    public static final String JBI_INSTALL_DIR = "addons";
}
