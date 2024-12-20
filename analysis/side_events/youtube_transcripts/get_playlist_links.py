from pytube import Playlist 

# Source to extract playlist link https://www.quora.com/How-do-I-get-all-share-URLs-from-a-Youtube-playlist-without-checking-each-video-page

### Extract the youtube video links
def get_video_links_from_playlist(playlist_url): 
    playlist = Playlist(playlist_url) 
    return [video.watch_url for video in playlist.videos]  
def write_to_file_and_display(video_urls, file_name='data/side_events/youtube_transcripts/youtube_video_links.txt'): 
    with open(file_name, 'w') as file: 
        for url in video_urls: 
            print(url) 
            file.write(url + '\n')  
# COP28 playlist link
playlist_url = 'https://www.youtube.com/playlist?list=PLBcZ22cUY9RLMkm-apVgzZ8JSi0Tsywd3' 
video_urls = get_video_links_from_playlist(playlist_url) 
write_to_file_and_display(video_urls)  
