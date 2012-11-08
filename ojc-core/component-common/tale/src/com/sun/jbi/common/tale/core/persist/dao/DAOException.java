package com.sun.jbi.common.tale.core.persist.dao;

public class DAOException extends Exception {
    private Throwable detail;

    /**
     * DAOException Constructor
     *
     * @param str String
     */
    public DAOException(String str) {
        super(str);
    }

    /**
     * DAOException Constructor
     *
     * @param str String
     * @param t Throwable
     */
    public DAOException(String str, Throwable t) {
        super(str, t);
        detail = t;
    }
    
    /**
     * DAOException Constructor
     *
     * @param t Throwable
     */
    public DAOException(Throwable t) {
        super(t.getMessage(), t);
        detail = t;
    }
}
