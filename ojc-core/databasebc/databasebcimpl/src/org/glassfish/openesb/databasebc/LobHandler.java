package org.glassfish.openesb.databasebc;

import java.io.IOException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Clob;
import java.sql.Blob;
import java.sql.SQLException;
import java.util.logging.Logger;

import javax.sql.rowset.serial.SerialBlob;
import javax.sql.rowset.serial.SerialClob;
import javax.sql.rowset.serial.SerialException;
import com.sun.jbi.common.util.Base64Utils;
import com.sun.jbi.internationalization.Messages;


/*
 * Uses JDBC 3.0 API for Handling LOBs.
 * Comes with limitations like Memory heap out of space.
 * Setters and getters for database CLOBs and BLOBs.
 */

public class LobHandler {

    private static final Messages mMessages = Messages.getMessages(LobHandler.class);
    
    public LobHandler() {
    }

    /**
     * Sets clob with the string value passed
     * @param columnNumber
     * @param value
     * @param ps
     * @throws SQLException
     * @throws SerialException
     */
    public static void setClob(int columnNumber, String value, final PreparedStatement ps)
            throws SQLException, SerialException {
        ps.setClob(columnNumber, new SerialClob(value.toCharArray()));
    }

    /**
     * Returns the clob as string.
     * @param columnNumber
     * @param rs
     * @return
     * @throws SQLException
     * @throws SerialException
     */
    public static String getClob(int columnNumber, final ResultSet rs, String driverName) throws SQLException, SerialException {
        if(driverName.toLowerCase().contains("oracle")){
        	java.sql.Clob clob = (java.sql.Clob) rs.getObject(columnNumber);
                if (clob != null){
                    String colValue = clob.getSubString(1, (int)clob.length());
        	    return colValue;
        	}else
            return null;
        }else {    	
	    	Clob clb = rs.getClob(columnNumber);
	        if (clb != null) {
	            return clb.getSubString((long) 1, (int) clb.length());
	        } else
	            return null;
        }
    }
    
    /**
     * Returns the clob as string.
     * @param columnNumber
     * @param rs
     * @return
     * @throws SQLException
     * @throws SerialException
     */
    public static String getClob(int columnNumber, final ResultSet rs) throws SQLException, SerialException {
        Clob clb = rs.getClob(columnNumber);
        if (clb != null) {
            return clb.getSubString((long) 1, (int) clb.length());
        } else
            return null;
    }

    /**
     * Sets blob with the byte[] from value.
     * @param columnNumber
     * @param value
     * @param ps
     * @throws SQLException
     * @throws SerialException
     */
    public static void setBlob(int columnNumber, String value, final PreparedStatement ps)
            throws SQLException, SerialException {
        ps.setBlob(columnNumber, new SerialBlob(value.getBytes()));
    }

    /**
     * Returns blob as a base64 string.
     * @param columnNumber
     * @param rs
     * @return
     * @throws SQLException
     * @throws SerialException
     * @throws IOException
     */
    public static String getBlob(int columnNumber, final ResultSet rs, String driverName) throws SQLException, SerialException,
            IOException {
    	if(driverName.toLowerCase().contains("oracle")){
        	java.sql.Blob blob = (java.sql.Blob)rs.getObject(columnNumber);
            if(blob!=null){
        		return new String(Base64Utils.byteToBase64String(blob.getBytes(1, (int)blob.length())));
        	}else
            return null;
        }else {
	        Blob blb = rs.getBlob(columnNumber);
	        if (blb != null) {
	            return Base64Utils.byteToBase64String(blb.getBytes((long) 1, (int) blb.length()));
	        } else 
			return null;
        }
    }
    
    /**
     * Returns blob as a base64 string.
     * @param columnNumber
     * @param rs
     * @return
     * @throws SQLException
     * @throws SerialException
     * @throws IOException
     */
    public static String getBlob(int columnNumber, final ResultSet rs) throws SQLException, SerialException,
            IOException {
        Blob blb = rs.getBlob(columnNumber);
        if (blb != null) {
            return Base64Utils.byteToBase64String(blb.getBytes((long) 1, (int) blb.length()));
        } else 
		return null;
    }

}