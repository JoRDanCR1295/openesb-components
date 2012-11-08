/*
 * JDBCOperations.java
 * 
 * Created on Sep 25, 2007, 5:56:33 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.databasebc;

/**
 *
 * @author narayana rallabandi
 */
public enum JDBCOperations {
    
    OPERATION_TYPE_INSERT,OPERATION_TYPE_UPDATE,OPERATION_TYPE_DELETE,OPERATION_TYPE_SELECT,
    OPERATION_TYPE_CREATE,OPERATION_TYPE_FIND,OPERATION_TYPE_POLL,OPERATION_TYPE_ALTER,
    OPERATION_TYPE_DROP,OPERATION_TYPE_TRUNCATE,OPERATION_TYPE_EXECUTE;
    
    @Override
    public String toString(){
        String operation = null;
        switch(this){
            case OPERATION_TYPE_INSERT:
                return "insert";
            case OPERATION_TYPE_UPDATE:
                return "update";
            case OPERATION_TYPE_DELETE:
                return "delete";
            case OPERATION_TYPE_SELECT:
                return "select";
            case OPERATION_TYPE_CREATE:
                return "create";
            case OPERATION_TYPE_FIND:
                return "find";
            case OPERATION_TYPE_POLL:
                return "poll";
            case OPERATION_TYPE_ALTER:
                return "alter";
            case OPERATION_TYPE_DROP:
                return "drop";
            case OPERATION_TYPE_TRUNCATE:
                return "truncate";
            case OPERATION_TYPE_EXECUTE:
                return "execute";
            default:
                return operation;
        }
    }

    public static final JDBCOperations getJDBCOperations(String opName){
        if(opName.equalsIgnoreCase("insert"))
            return OPERATION_TYPE_INSERT;
        if(opName.equalsIgnoreCase("update"))
            return OPERATION_TYPE_UPDATE;
        if(opName.equalsIgnoreCase("delete"))
            return OPERATION_TYPE_DELETE;
        if(opName.equalsIgnoreCase("select"))
            return OPERATION_TYPE_SELECT;
        if(opName.equalsIgnoreCase("create"))
            return OPERATION_TYPE_CREATE;
        if(opName.equalsIgnoreCase("find"))
            return OPERATION_TYPE_FIND;
        if(opName.equalsIgnoreCase("poll"))
            return OPERATION_TYPE_POLL;
        if(opName.equalsIgnoreCase("drop"))
            return OPERATION_TYPE_DROP;
        if(opName.equalsIgnoreCase("truncate"))
            return OPERATION_TYPE_TRUNCATE;
        else                            
            return OPERATION_TYPE_EXECUTE;
        //return null;
    }
    
    public static final String getOpType(String sqlText){
        String[] st = sqlText.split("\\s");
        return st[0];
    }
    
    public static void main(String[] args){
        //JDBCOperations jdbcops = new JDBCOperations();
        System.out.print(JDBCOperations.valueOf("OPERATION_TYPE_ALTER"));//.getJDBCOperations("alter"));
        System.out.println("\n\n"+JDBCOperations.OPERATION_TYPE_ALTER.toString());
        System.out.println("\n\n"+JDBCOperations.OPERATION_TYPE_UPDATE.toString().equals("update"));
    }
}
