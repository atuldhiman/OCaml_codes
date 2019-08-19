import java.io.Console;
    import java.util.Random;

    class QuickSorter_par implements Runnable{
        int[] data;
        int start,end;
        QuickSorter_par(int[] data,int start,int end) {
            this.data=data;
            this.start=start;
            this.end=end;
        }
        public void run(){
            quickSort(this.data,this.start,this.end);
        }
        static void swap(int[] data, int i, int j){
            int tmp=data[i];
            data[i]=data[j];
            data[j]=tmp;
        }
        static int partition(int[] data, int start, int end) {
            if(start==end)
                return start;
            int pivot=data[end];
            int s=start-1;
            for(int i=start;i<end;i++)
                if(data[i]<=pivot)
                    swap(data,++s,i);
            swap(data,++s,end);
            return s;
        }
        static void quickSort(int[] data, int start, int end) {
            if (end<=start)
                return;
            int s=partition(data,start,end);
            quickSort(data,start,s-1);
            quickSort(data,s+1,end);
        }
        static int[] randomList(int n,int k) {
            Random random=new Random();
            int[] data=new int[n];
            for(int i=0;i<data.length;i++)
                data[i]=random.nextInt(k);
            return data;
        }
        public static void main(String[] args) {
            // int[] data={3,1,2,8,5,6};
            // quickSort(data,0,data.length-1);
            // for(int i:data)
            //     System.console().format("%d ",i);
            // System.console().format("\n");
            int n=10000000;
            if (args.length > 0) n = Integer.parseInt (args[0]);
            int[] data=randomList(n,1000000);
            int s=partition(data,0,n-1);
            Thread t1=new Thread(new QuickSorter_par(data,0,s-1));
            Thread t2=new Thread(new QuickSorter_par(data,s+1,data.length-1));
            t1.start();
            t2.start();
            try {
                t1.join();
                t2.join();
            }catch(InterruptedException e){
                System.out.println(e);
            }
        }
    }