package com.sun.jbi.common.tale.core.persist.dao;

import com.sun.jbi.common.tale.core.connection.DBConnectionFactory;

public class DerbyDAO extends DAO {
    public DerbyDAO(DBConnectionFactory connFactory) throws DAOException {
        super(connFactory);
    }
}
