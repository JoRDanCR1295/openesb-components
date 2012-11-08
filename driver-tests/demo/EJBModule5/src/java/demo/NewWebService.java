/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package demo;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebService;
import javax.ejb.Stateless;

/**
 *
 * @author jqian
 */
@WebService()
@Stateless()
public class NewWebService {

    /**
     * Web service operation
     */
    @WebMethod(operationName = "echo")
    public String echo(@WebParam(name = "parameter")
    String parameter) {
        //TODO write your implementation code here:
        return null ;
    }

}
