/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package persistence;

import java.sql.Connection;
import java.sql.Statement;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 *
 * @author mbhasin
 */
public class DBConnection {

    Connection conn = null;

    public DBConnection() {
        try {
            Class.forName("com.mysql.jdbc.Driver").newInstance();
        } catch (Exception E) {
            System.err.println("Unable to load driver.");
            E.printStackTrace();
        }

        conn = null;
        try {
            conn = DriverManager.getConnection("jdbc:mysql://localhost/wlm?user=root&password=");
        /**
         * for debug
         * stmt = conn.createStatement();
         * rs = stmt.executeQuery("SELECT * from Instance");
         * while (rs.next()) {
         * System.out.println("outputting row.. " + rs.getString(1));
         * }
         */
        } catch (SQLException E) {
            System.out.println("SQLException: " + E.getMessage());
            System.out.println("SQLState:     " + E.getSQLState());
            System.out.println("VendorError:  " + E.getErrorCode());
            closeConnection();
        } 
    }

    public Connection getDBConnection() {
        return conn;
    }

    public void closeConnection() {
        try {
            if (conn != null) {
                conn.close();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void closeResultset(ResultSet rs) {
        try {
            if (rs != null) {
                rs.close();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
