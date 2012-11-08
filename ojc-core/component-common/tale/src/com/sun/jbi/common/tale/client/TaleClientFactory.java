/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)TaleClientFactory.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.client;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.common.tale.client.impl.DefaultALEClient;
import com.sun.jbi.common.tale.core.connection.ConnectionProperties;
import com.sun.jbi.common.tale.core.domain.TaleDomain;
import com.sun.jbi.common.tale.core.domain.TaleRequest;
import com.sun.jbi.common.tale.core.domain.SourceInfo;
import com.sun.jbi.common.tale.core.util.TaleConfigurationException;
import com.sun.jbi.common.tale.core.util.I18n;

/**
 * Factory to create {@link TaleClient}s.
 * @author Kevan Simpson
 */
public class TaleClientFactory {
    public static final String DEFAULT_TALECLIENT_CONFIG = "tale-client-config.properties";
    
    private static Logger mLogger = Logger.getLogger(TaleClientFactory.class.getName());
    
    /**
     * Instantiates a new {@link TaleClient} using the default configuration.
     * @return An TaleClient.
     */
    public static TaleClient newALEClient() {
        FileInputStream inStream = null;
        File file = null;
        try {
            file = new File(DEFAULT_TALECLIENT_CONFIG);
            inStream = new FileInputStream(file);
        }
        catch (FileNotFoundException fnfe) {
            String msg = I18n.loc(
                    "TALE-7017: Cannot find TaleClient configuration file at {0}", 
                    file.getAbsolutePath());
            mLogger.log(Level.SEVERE, msg, fnfe);
            throw new TaleConfigurationException(msg, fnfe);
        }
        
        Properties props = new Properties();
        try {
            props.load(inStream);
        }
        catch (IOException ioe) {
            String msg = I18n.loc(
                    "TALE-7018: Failed to load TaleClient configuration file at {0}: {1}", 
                    file.getAbsolutePath(), ioe.getMessage());
            mLogger.log(Level.SEVERE, msg, ioe);
            throw new TaleConfigurationException(msg, ioe);
        }
        
        return newALEClient(props);
    }
    
    /**
     * Instantiates a new {@link TaleClient} using the specified configuration and context.
     * <p>
     * The following properties are expected to be defined in the specified configuration:
     * <ul>
     *      <li>{@link ConnectionProperties#DB_TYPE}</li>
     *      <li>{@link ConnectionProperties#DB_INSTANCE}</li>
     *      <li>{@link ConnectionProperties#DB_HOST}</li>
     *      <li>{@link ConnectionProperties#DB_PORT}</li>
     *      <li>{@link ConnectionProperties#DB_USERNAME}</li>
     *      <li>{@link ConnectionProperties#DB_PASSWORD}</li>
     * </ul></p>
     * @param props TaleClient configuration.
     * @return An TaleClient.
     */
    public static TaleClient newALEClient(Properties props) {
        //ToDo: Clean-up domain code
        //return new DefaultALEClient(TaleDomain.newInstance(props));
        return new DefaultALEClient(null);
    }
    
    /*
     * This only works if you clear out the following tables, in order:
            delete from ALESE_USER.CSF_PAYLOAD_STORE;
            delete from ALESE_USER.CSF_PAYLOAD_LOG;
            delete from ALESE_USER.CSF_CME_LOG;
            delete from ALESE_USER.CSF_LOGGER_LOG;
     */
    public static void main(String[] args) {
        try {
            Properties p = new Properties();
            p.setProperty(ConnectionProperties.DB_TYPE, "Apache Derby");
            p.setProperty(ConnectionProperties.DB_INSTANCE, "aleseDB");
            p.setProperty(ConnectionProperties.DB_HOST, "localhost");
            p.setProperty(ConnectionProperties.DB_PORT, "1527");
            p.setProperty(ConnectionProperties.DB_USERNAME, "alese_user");
            p.setProperty(ConnectionProperties.DB_PASSWORD, "alese_user");
            
            TaleClient client = TaleClientFactory.newALEClient();
            TaleRequest req = client.newRequest();
            req.setCode(100);
            req.setDetails("bogus test");
            SourceInfo info = req.getSourceInfo();
            info.setApplicationType("MyAppType");
            info.setModuleName("MN");
            info.setServiceName("SN");
            info.setUnitName("UN");
            client.sendLog(req);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }
//    */
}
