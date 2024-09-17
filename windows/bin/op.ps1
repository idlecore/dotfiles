$path = fzf --walker-root=C:\syncthing --walker=dir,follow,hidden
if ($path)
{
    nvim $path -c ":cd $path"
}
