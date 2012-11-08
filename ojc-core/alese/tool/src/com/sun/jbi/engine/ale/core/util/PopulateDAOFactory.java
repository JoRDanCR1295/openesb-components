package com.sun.jbi.engine.ale.core.util;

import java.util.Properties;

/**
 * PopulateDAOFactory.java
 *
 * @author Edward Chou
 */
public class PopulateDAOFactory {

    private static final String DB_TYPE_PROP = "DB_Type";
    private static final String DB_HOST_PROP = "DB_Host";
    private static final String DB_PORT_PROP = "DB_Port";
    private static final String DB_INSTANCE_PROP = "DB_Instance";
    private static final String DB_USERNAME_PROP = "DB_Username";
    private static final String DB_PASSWORD_PROP = "DB_Password";
        
    public static PopulateDAO createDAO(Properties props) throws Exception {
        String dbType = props.getProperty(DB_TYPE_PROP);
        PopulateDAO dao = null;
        if ("MySQL".equalsIgnoreCase(dbType)) {
            dao = new MySQLDAO();
        } else {
            throw new Exception("unsupported DB type: " + dbType);
        }
        
        dao.setHost(props.getProperty(DB_HOST_PROP));
        dao.setPort(props.getProperty(DB_PORT_PROP));
        dao.setInstance(props.getProperty(DB_INSTANCE_PROP));
        dao.setUsername(props.getProperty(DB_USERNAME_PROP));
        dao.setPassword(props.getProperty(DB_PASSWORD_PROP));
        
        dao.init();
        return dao;
    }
    
}
