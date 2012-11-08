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
 * @(#)Test.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.Calendar;

import test.PurchaseOrderService.PurchaseOrderPortType_Stub;
import test.PurchaseOrderService.PurchaseOrderService;
import test.PurchaseOrderService.PurchaseOrderService_Impl;
import test.PurchaseOrderService.PurchaseOrderType;
import test.PurchaseOrderService.Customer;
import test.PurchaseOrderService.Item;
import test.PurchaseOrderService.Items;
import test.PurchaseOrderService.Status;
import test.PurchaseOrderService.USAddress;


public class Test {

	/**
	 * @param args
	 */
	public static void main(String[] args) throws Exception {
		// TODO Auto-generated method stub
		if (args.length != 4) {
			System.out.println("Usage: Test service-URL #threads #iterations verifyFlag");
			return;
		}
		PurchaseOrderService poService = new PurchaseOrderService_Impl();

		PurchaseOrderPortType_Stub poStub = 
                        (PurchaseOrderPortType_Stub) (poService.getPurchaseOrderPort());
		String serviceURL = args[0];
		poStub._setProperty(javax.xml.rpc.Stub.ENDPOINT_ADDRESS_PROPERTY,
				serviceURL); // http://localhost:18181/purchaseOrderService/purchaseOrderPort

		int threads = Integer.parseInt(args[1]);
		int count = Integer.parseInt(args[2]);
                String strVerify = args[3];
                Boolean verify = 
                        (Boolean)("true".equals(strVerify)); 
                
                // build up PO data off-line
                Calendar orderDate = Calendar.getInstance(); 
                Calendar shipDate  = Calendar.getInstance();
                orderDate.set(2006,11,07);
                shipDate.set(2006,12,02);
                Customer customer  = new Customer();
                customer.setName("John Doe");                
                customer.setAddress( new USAddress( "US",
                                "John Doe", "123 Main street", "Monrovia", 
                                "CA", new BigDecimal("91016")) );                       
                USAddress shipTo   = new USAddress( "US",
                                "John Doe", "123 Main street", "Arcadia", 
                                "CA", new BigDecimal("91007"));
                USAddress billTo   = new USAddress( "US",
                                "John Doe", "123 Main street", "Duarte", 
                                "CA", new BigDecimal("91010"));
                Status status = null;
                String comment = new String("Order my books");
                Item it[] = { new Item("123", "Book1", 
                                new BigInteger("1"), 
                                new BigDecimal(50.00), 
                                "byAbc", shipDate) };
                Items lineItems = new Items();
                lineItems.setItem(it);
 
                // build and load up worker threads
                Worker runObjs[] = new Worker[threads];
		for (int i = 0; i < threads; i++) {
			runObjs[i] = new Worker();
			runObjs[i].count = count;
			runObjs[i].stub = poStub;
                        runObjs[i].mVerify = verify;
                        runObjs[i]._orderDate = orderDate;
                        runObjs[i]._shipDate = shipDate;
                        runObjs[i]._customer = customer;
                        runObjs[i]._shipTo = shipTo;
                        runObjs[i]._billTo = billTo;
                        runObjs[i]._status = status;
                        runObjs[i]._comment = comment;
                        runObjs[i]._lineItems = lineItems;
		}
                
                // start test run
                ExecutorService es = 
                        Executors.newFixedThreadPool(threads);
                long time = System.currentTimeMillis();
		for (int i = 0; i < threads; i++) {
			es.submit(runObjs[i]);
		}
		es.shutdown();
		es.awaitTermination(5760l, TimeUnit.SECONDS);
		long time1 = System.currentTimeMillis();
		float result = (float)(threads * count * 1000) / (time1 - time);
		System.out.println(
                        "Time=" + (time1 - time) + " Msg=" + (threads * count));
		System.out.println(
                        "Test completed. #threads: " + threads +
                        "  #Iterations: " + count +
                        " throughput: " + result +
                        " msgs/sec");

	}

}

class Worker implements Runnable {
	int count = 0;
        Boolean mVerify = true;
	PurchaseOrderPortType_Stub stub = null;
        Calendar   _orderDate = null;
        Calendar   _shipDate = null;
        Customer   _customer = null;
        USAddress  _shipTo = null;
        USAddress  _billTo = null;
        Status     _status = null;
        String     _comment = null; 
        Items      _lineItems = null;

	public void run() {
		int i = 0;
		try {    
                        if (mVerify) { // this case verifies that the correct data is returned
                            for (; i < count; i++) {
                                PurchaseOrderType responseObj =
                                        stub.purchaseOrderOperation( _orderDate,
                                        _customer, _shipTo, _billTo, 
                                        _status, _comment, _lineItems );
                                System.out.println("Response is : " +         
                                (boolean)(
                                ( responseObj.getOrderDate().YEAR == _orderDate.YEAR ) & 
                                ( responseObj.getOrderDate().MONTH == _orderDate.MONTH ) &
                                ( responseObj.getOrderDate().DATE == _orderDate.DATE ) &
                                ( "John Doe".equals( responseObj.getCustomer().getName() ) ) &                  
                                ( "US".equals( responseObj.getCustomer().getAddress().getCountry() ) ) &
                                ( "John Doe".equals( responseObj.getCustomer().getAddress().getName() ) ) &
                                ( "123 Main street".equals( responseObj.getCustomer().getAddress().getStreet() ) ) &
                                ( "Monrovia".equals( responseObj.getCustomer().getAddress().getCity() ) ) &
                                ( "CA".equals( responseObj.getCustomer().getAddress().getState() ) )&
                                ( "91016".equals( responseObj.getCustomer().getAddress().getZip().toString() ) ) &
                                ( "US".equals( responseObj.getShipTo().getCountry() ) ) &
                                ( "John Doe".equals( responseObj.getShipTo().getName() ) ) &
                                ( "123 Main street".equals( responseObj.getShipTo().getStreet() ) ) &
                                ( "Arcadia".equals( responseObj.getShipTo().getCity() ) ) &
                                ( "CA".equals( responseObj.getShipTo().getState() ) ) &
                                ( "91007".equals( responseObj.getShipTo().getZip().toString() ) ) &
                                ( "US".equals( responseObj.getBillTo().getCountry() ) ) &
                                ( "John Doe".equals( responseObj.getBillTo().getName() ) ) &
                                ( "123 Main street".equals( responseObj.getBillTo().getStreet() ) ) &
                                ( "Duarte".equals( responseObj.getBillTo().getCity() ) ) &
                                ( "CA".equals( responseObj.getBillTo().getState() ) ) &
                                ( "91010".equals( responseObj.getBillTo().getZip().toString() ) ) &
                                ( "InProgress".equals( responseObj.getStatus().toString() ) ) &
                                ( "Order my books".equals( responseObj.getComment() ) ) &
                                ( "123".equals( ((Item)(responseObj.getLineItems().getItem())[0]).getPartNum() ) ) &                 
                                ( "Book1".equals( ((Item)(responseObj.getLineItems().getItem())[0]).getProductName() ) ) &
                                ( ((Item)(responseObj.getLineItems().getItem())[0]).getQuantity().equals( new BigInteger("1") ) ) &
                                ( ((Item)(responseObj.getLineItems().getItem())[0]).getUSPrice().equals( new BigDecimal(50.00) ) ) &
                                ( "byAbc".equals(((Item)(responseObj.getLineItems().getItem())[0]).getComment() ) ) & 
                                ( ((Item)(responseObj.getLineItems().getItem())[0]).getShipDate().YEAR  == _shipDate.YEAR  ) &
                                ( ((Item)(responseObj.getLineItems().getItem())[0]).getShipDate().MONTH == _shipDate.MONTH ) &
                                ( ((Item)(responseObj.getLineItems().getItem())[0]).getShipDate().DATE  == _shipDate.DATE  )  ) );
                            }
                        
                        }else{ // this part runs quietly for the benchmark run
                            for (; i < count; i++) {
                                PurchaseOrderType responseObj =
                                        stub.purchaseOrderOperation( _orderDate,
                                        _customer, _shipTo, _billTo, 
                                        _status, _comment, _lineItems );
                            }
                        } 

		} catch (Exception e) {
			System.out.println("Ran into exceptions at count " + (i+1));
			e.printStackTrace();
		}
	}
}
