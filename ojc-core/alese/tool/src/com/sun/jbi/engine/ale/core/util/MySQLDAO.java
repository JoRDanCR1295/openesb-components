package com.sun.jbi.engine.ale.core.util;

/**
 * MySQLDAO.java
 *
 * @author Edward Chou
 */
public class MySQLDAO extends PopulateDAO {

    private static final String JDBC_DRIVER_CLASS_NAME = "com.mysql.jdbc.Driver";
    private static final String DB_TYPE = "mysql";
    
    public MySQLDAO() throws Exception {
        this.setJdbcDriverClassName(JDBC_DRIVER_CLASS_NAME);
        this.setDbType(DB_TYPE);
    }
    
}
