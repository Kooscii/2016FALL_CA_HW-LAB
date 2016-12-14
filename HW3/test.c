int main()
{
	int b[0777];
	int i=0;
	for(i=0;i<0777;i++){
		if(i%2==0){
			b[i]=2;
		}
		if(i%4==0){
			b[i]=1;
		}
	}
}
